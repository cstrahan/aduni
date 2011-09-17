/**
   Gateway.java

   @author JMR
   @version $Id: Gateway.java,v 1.32 2001/01/28 16:33:22 jeff Exp $

   The gateway is responsible for making sure messages are properly
   and securely parsed. When it is convinced the packet and its 
   contents are good, it will create an object of the appropriate type
   and send it to the appropriate place. This is very fidgety.
*/   

import java.util.*;
import java.net.*;

public class Gateway {


  /**
     Sets the control handler
     @param c The ControlHandler
  */
  public static void setControlHandler(ControlHandler c) {
    control = c;
  }

  /**
     Sets the Query handler
     @param q the QueryHandler
  */
  public static void setQueryHandler(QueryHandler q) {
    query = q;
  }

  /**
     Sets the QueryHitHandler
     @param q the QueryHitHandler
  */
  public static void setQueryHitHandler(QueryHitHandler q) {
    queryHit = q;
  }
  
  /**
     Reads information about an incoming descriptor header,
     parses the bytes into the right object, and sends it on.
     Enforces with no mercy.
     @param header a 23 byte decriptor header candidate.
     @param socketId the socket from which the header came.
     @return <code>true</code> on success
  */
  public static boolean parseMessage(byte[] header, int socketId) 
  {
    byte[] messageId = new byte[16];
    try {
      // Grab the messageId
      System.arraycopy(header, 0, messageId, 0, 16);

      // Check it with the table, and add if it necessary
      // *********** CODE GOES HERE **************
    }
    catch(Exception e) { 
      control.reportErr("parseDescriptorHeader(): Bad messageId");
      return false; // bail out
    } 
  
    // parse and check the payload descriptor
    int payloadDescriptor = header[16];

    // <<< THIS IS NOT ELEGANT >>>
    if(payloadDescriptor != 0x40 && payloadDescriptor != 0x00 &&
       payloadDescriptor != 0x01 && payloadDescriptor != 0x80 &&
       payloadDescriptor != 0x81 && payloadDescriptor != -128 &&
       payloadDescriptor != -127) {
      control.reportErr("Unknown descriptor, bailing out.");
      return false; // bail out
    }
    
    // Grab ttl
    int ttl = header[17];    
    // ***************** CHECK TTL RANGE **********************

    // Grab hops
    int hops = header[18];

    // ****************** CHECK HOPS AGAINST TTL **************

    // Grab payload
    int payloadLength = Utility.deserializeInt(header, 19);
    
    /*
      At this point, the descriptor checks out, and we're good to go. Now
      to create the appropriate object, check the payload, and send it 
      to the right handler
    */

    control.report
      (new String("Message from " + MessageServer.getAddress(socketId)));

    // makes a storage array for the payload
    byte payload[] = new byte[payloadLength];
    if(payloadLength != 0)
    {
      if(payloadLength > 5000) // an arbitrary number, but unlikely.
      {
        control.reportErr(new String ("Payload too large, ignoring."));
        return false;
      }
      MessageServer.getBytes(payload, payloadLength, socketId);
    }

    // ------------------------- PING -------------------------------
    if(payloadDescriptor == Utility.PING) {
      if(payloadLength != 0) return false; // double check

      // creates a new Ping object
      Ping message = new Ping(messageId, payloadDescriptor, 
                              ttl, hops, payloadLength);
      message.setSocketId(socketId);

      // send it to the PingHandler
      control.report(message);
      PingHandler.receivePing(message);
    }

    // ------------------------- PONG -------------------------------
    else if(payloadDescriptor == Utility.PONG) {
      // parse the payload
      if(payloadLength != 14)
        control.reportErr(new String("Payload wrong size, bailing out."));
      
      int port = Utility.deserializePort(payload, 0);

      byte[] ipAddress = new byte[4];
      System.arraycopy(payload, 2, ipAddress, 0, 4);
      int sharedFiles = Utility.deserializeInt(payload, 6);
      int kilobytes = Utility.deserializeInt(payload, 10);
      
      // create a Pong object
      Pong message = new Pong(messageId, payloadDescriptor, 
                              ttl, hops, payloadLength,
                              port, ipAddress, sharedFiles, kilobytes);
      message.setSocketId(socketId);

      // send it to the PongHandler
      control.report(message);
      PongHandler.receivePong(message);
    }

    // ------------------------- QUERY --------------------------------
    else if(payloadDescriptor == 0x80 || payloadDescriptor == -128) {
      // parse the payload
      short minSpeed = (short)Utility.deserializePort(payload, 0);
      String searchString = "";
      for(int i = 2; i < payloadLength; i++) 
        searchString += (char)payload[i];
      searchString.trim();

      if(searchString.length() < 2) {
        control.reportErr("Search string too small, discarding.");
        return false;
      }

      // create a Query object
      QueryObject message = new QueryObject(messageId, payloadDescriptor, 
                              ttl, hops, payloadLength, minSpeed,
                                            searchString);
      message.setSocketId(socketId);

      // send it to the QueryHandler
      control.report(message);
      query.startSearch(message);
    }

    // --------------------- QUERY_HIT -------------------------------
    else if(payloadDescriptor == 0x81 || payloadDescriptor == -127) {

      // get the number of hits
      int numHits = (int)payload[0];
      
      // get the port Number
      short portNum = (short)Utility.deserializePort(payload, 1);

      // get the IP address
      byte[] ipAddress = new byte[4];
      System.arraycopy(payload, 3, ipAddress, 0, 4);

      // get the speed
      int speed = Utility.deserializeInt(payload, 7);      
      
      // Put together the result set
      int offset = 11;
      Vector resultSet = new Vector();
      for (int i = 0 ; i < numHits; i++)
      {
        int fileIndex = Utility.deserializeInt(payload, offset);
        offset += 4;
        int fileSize = Utility.deserializeInt(payload, offset);
        offset += 4;
        String fileName = Utility.grabString(payload, offset);
        offset += (fileName.length() + 2);
        resultSet.add(new ResultObject(fileIndex, fileSize, fileName));
      }

      // get the servent ID
      byte[] serventId = new byte[16];
      System.arraycopy(payload, (payloadLength - 16), serventId, 0, 16);

      // create a QueryHit object
      QueryHitObject message = 
        new QueryHitObject(messageId, payloadDescriptor, 
                           ttl, hops, payloadLength, numHits, portNum,
                           ipAddress, speed, resultSet,
                           serventId);
      message.setSocketId(socketId);

      // send it to the QueryHitHandler
      control.report(message);
      queryHit.reportResult(message);
            
    }
      
    // ------------------------- PUSH -------------------------------
  
    else if(payloadDescriptor == Utility.PUSH) {
      // parse the payload
      byte[] serventID = new byte[16];

      int fileIndex;
      byte[] ipAddress = new byte[4];
      int port;
    
      // create a Push object
      /*        
                Push message = new Push(messageId, payloadDescriptor, 
                ttl, hops, payloadLength, serventID,
                fileIndex, ipAddress, port); 
                message.setSocketId(socketId); 
      */
      // send it to the PushHandler
      /*      
              control.report(message);
              PushHandler.receivePush(message);
      */
    }

    else {
      control.reportErr(new String("extractPayload(): Cannot identify payload"));
      return false;
    }
    return true;
  } // end method parseMessage (finally!)


  /* ------------------------------------------------------------------------ *
   *                                                                          *
   *                         (take a break)                                   *
   *                                                                          *
   * ------------------------------------------------------------------------ */

  /**
     This extracts the socketId from the object, and sends it to the larger 
     deliver() method
     @param object The object to deliver
     @return true on success
  */
  public static boolean deliver(MessageObject object)
  {
    return deliver(object, object.getSocketId());
  }

  /**
     Deliver is the reverse of parseMessage. It takes an object, converts it
     to bytes, and sends it on.
     @param object The object to deliver
     @param sock The socketId to deliver to
     @return true on success
  */
  public static boolean deliver(MessageObject object, int sock) {
    byte[] buffer = new byte[1024];
    int payloadDescriptor = object.getPayloadDescriptor();
    int socketId = sock;

    //clear out the buffer. Probably don't need to do this
    for(int i = 0; i < 1024; i++)
      buffer[i] = (byte)0;
    int offset = 23;

    // construct the basic header. much of this will be
    // overwritten further on.
    constructHeader(buffer, object);

    control.report(new String
      ("Gateway: delivering " + object + " to " + 
       MessageServer.getAddress(socketId) + " => "));

    /* Constructs the appropriate payload, according to type */
    if(payloadDescriptor == Utility.PING) {} 
    else if (payloadDescriptor == 1) {
        offset = constructPongPayload(buffer, object);
    }
    else if (payloadDescriptor == Utility.QUERY || payloadDescriptor == -181) {
        offset = constructQueryPayload(buffer, object);        
    }
    else if (payloadDescriptor == Utility.QUERY_HIT || payloadDescriptor == -182) {
        offset = constructQueryHitPayload(buffer, object);
    }
    else if (payloadDescriptor == Utility.PUSH) {
        offset = constructPushPayload(buffer, object);
    }
    else {
      control.reportErr("Error delivering: cannot identify object type");
      return false;
    }

    /* Send the message on to the socket, and report success (hopefully) */
    if(send(buffer, offset, payloadDescriptor, socketId))
      return true;
    else {
      control.reportErr(new String("An unknown error occured in sending package."));
      return false;
    }
  } // end deliver method


  /* -----------------------------------------------------------------------
   * The following methods do most of the work of deconstructing the Object 
   * into a byte stream.
   * --------------------------------------------------------------------- */

  /**
     Constructs the descriptor Header.
  */
  private static boolean constructHeader(byte[] buffer, MessageObject object) 
  {
    // place the header
    byte[] id = object.getMessageId();
    System.arraycopy(id, 0, buffer, 0, 16);
    
    // payload decriptor, ttl and hops 
    buffer[16] = (byte)object.getPayloadDescriptor();
    buffer[17] = (byte)object.getTtl();
    buffer[18] = (byte)object.getHops();

    // payload length
    int payloadLength = object.getPayloadLength();
    Utility.serializeInt(payloadLength, buffer, 19);
    return true;
  }

  /**
     Constructs a paylod for a pong message
  */
  private static int constructPongPayload(byte[] buffer, MessageObject object) {
    int offset = 23;
    Pong aPong = (Pong)object;  // cast it as a pong
    buffer[16] = 0x01;
    offset = Utility.serializePort(aPong.getPort(), buffer, offset);
    byte[] ipAddress = aPong.getIpAddress();
    System.arraycopy(ipAddress, 0, buffer, offset, 4);
    offset += 4;
    offset = Utility.serializeInt(aPong.getSharedFiles(), buffer, offset);
    offset = Utility.serializeInt(aPong.getKilobytes(), buffer, offset);

    return offset;
  }

  /**
     Constructs a payload for a query message
  */
  private static int constructQueryPayload(byte[] buffer, MessageObject object) {
    QueryObject aQuery = (QueryObject)object; // cast

    int offset = 23;
    
    // set the payload descriptor
    buffer[16] = (byte)0x80;

    // set the speed
    short minSpeed = aQuery.getMinSpeed();
    offset = Utility.serializePort(minSpeed, buffer, offset);

    // set the search string
    String searchString = aQuery.getSearchString();
    for(int i = 0; i < searchString.length(); i++)
      buffer[offset++] = (byte)searchString.charAt(i);
    buffer[offset++] = 0;

    // set the payload length
    Utility.serializeInt((offset - 23), buffer, 19);

    // return the total size
    return offset;
  }

  /**
     Constructs a payload for a query hit message
  */
  private static int constructQueryHitPayload(byte[] buffer, MessageObject object) {
    QueryHitObject aQueryHit = (QueryHitObject)object; //cast as QueryHitObject
    int offset = 23;

    // set the proper payload descriptor
    buffer[16] = (byte) 0x81;   
    Vector resultSet = (Vector)aQueryHit.getResultSet();

    // add number of hits
    int numHits = resultSet.size();
    buffer[offset++] = (byte)numHits;
    
    // add port
    offset = Utility.serializePort(aQueryHit.getPortNum(), buffer, offset);

    // add ip address
    byte[] ip = Utility.getIp();
    System.arraycopy(ip, 0, buffer, offset, 4);
    offset += 4;
    
    // add speed
    offset = Utility.serializeInt(aQueryHit.getSpeed(), buffer, offset);

    // deconstruct the resultset
    for(int i = 0; i < resultSet.size(); i++)
    {
      ResultObject tmpResObj = (ResultObject)resultSet.get(i);
      // copy file index
      int fileIndex = tmpResObj.getFileIndex();
      offset = Utility.serializeInt(fileIndex, buffer, offset);

      // copy file size
      int fileSize = tmpResObj.getFileSize();
      offset = Utility.serializeInt(fileSize, buffer, offset);

      // copy file name
      String fileName = tmpResObj.getName();
      fileName.trim();
      for(int j = 0; j < fileName.length(); j++)
        buffer[offset++] = (byte)fileName.charAt(j);
      buffer[offset++] = 0;
      buffer[offset++] = 0;
    }

    // add the servent ID
    System.arraycopy(aQueryHit.getServentID(), 0, buffer, offset, 16);
    offset += 16;
    
    // Insert the proper payload length
    Utility.serializeInt((offset - 23), buffer, 19);

    // return the total length of the buffer
    return offset;
  }

  /**
     Constructs a payload for a push message
  */
  private static int constructPushPayload(byte[] buffer, MessageObject object) {
    
    int offset = 23;

    // 
    //
    // <<< IMPLEMENT THIS >>>
    //
    //

    return offset;
  }

  /**
     Sends the message on to the appropriate place. Soon will be replaced with method call to
     control...
  */
  private static boolean send(byte[] buffer, int offset, int payloadDescriptor, int socketId) {
    MessageServer.dispatch(buffer, offset, payloadDescriptor, socketId);
    return true;
  }

  // fields
  private static MessageServer server;         // the server 
  private static ControlHandler control;       // the control handler
  private static QueryHandler query;           // the query handler
  private static QueryHitHandler queryHit;     // the query hit handler
 }
