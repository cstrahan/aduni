/**
   Gateway.java

   The gateway is responsible for making sure messages are properly
   and securely parsed. When it is convinced the packet and its 
   contents are good, it will create an object of the appropriate type
   and send it to the appropriate place.

   @author JMR
   @version $Id: Gateway.java,v 1.7 2001/01/23 19:46:30 jeff Exp $
*/   

public class Gateway
{
  /**
     Right now, this just reads information about an incoming 
     descriptor header. Minimal security
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
      System.out.println("parseDescriptorHeader(): Bad messageId");
      return false; // bail out
    } 
  
    // parse and check the payload descriptor
    int payloadDescriptor = (int)header[16];
    if(payloadDescriptor != 0x40 && payloadDescriptor != 0x00 &&
       payloadDescriptor != 0x01 && payloadDescriptor != 0x80 &&
       payloadDescriptor != 0x81) {
      System.out.println(" Unknown descriptor, bailing out.");
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
      to create the appropriate object and send it to the right handler

      MessageObject temp = new MessageObject(messageId, payloadDescriptor, 
      ttl, hops, payloadLength);
      temp.setSocketId(socketId);

    */

    System.out.print("Descriptor header successfully parsed!");
    System.out.println("Attempting to extract payload...");

    /**
       Attempts to parse and extract the payload.
    */
    byte payload[] = new byte[payloadLength];
    MessageServer.getBytes(payload, 0, socketId);

    // ------------------------- PING -------------------------------
    if(payloadDescriptor == Utility.PING) {
      if(payloadLength != 0) return false; // double check

      // creates a new Ping object
      Ping message = new Ping(messageId, payloadDescriptor, 
                              ttl, hops, payloadLength);

      // send it to the PingHandler
      System.out.println(message);
    }

    // ------------------------- PONG -------------------------------
    else if(payloadDescriptor == Utility.PONG) {
      // parse the payload
      int port = (int)payload[0];
      // IP?
      int sharedFiles = Utility.deserializeInt(payload, 6);
      int kilobytes = Utility.deserializeInt(payload, 10);
      
      // create a Pong object
      // Pong message = new Pong();

      // send it to the PongHandler
    }

    // ------------------------- QUERY --------------------------------
    else if(payloadDescriptor == Utility.QUERY) {
      // parse the payload
      short minSpeed = (short)payload[0];
      String searchString = "";
      for(int i = 1; i == payloadLength; i++)
        searchString += (char)payload[i];
      
      // create a Query object
      // QueryObject message = new OueryObject();

      // send it to the QueryHandler
    }

    // --------------------- QUERY_HIT -------------------------------
    else if(payloadDescriptor == Utility.QUERY_HIT) {
      // parse the payload
      int numHits = (int)payload[0];
      int portNum = (int)payload[1];
      // IP?
      int speed = deserializeInt(payload, 7);
      
      // Put together the result set
      ArrayList resultSet;
      

      byte[] serventId = new byte[16];
      System.arraycopy(payload, (payloadLength - 16), serventId, 0, 16);

      // create a QueryHit object
      // QueryHitObject message = new QueryHitObject();

      // send it to the QueryHitHandler
    }

    // ------------------------- PUSH -------------------------------
    else if(payloadDescriptor == Utility.PUSH) {
      // parse the payload

      // create a Push object

      // send it to the PushHandler
    }
    else {
        System.out.println("extractPayload(): Cannot identify payload");
        return false;
    }
    return true;
  } // end method parseMessage

  
  public static boolean deliver(MessageObject object) {
    byte[] buffer = new byte[23];
    int offset  = 23;
    int payload = object.getPayloadDescriptor();
    int socketId = object.getSocketId();

    if(payload == Utility.PING) 
    {
      // convert and send to all but the original
      System.out.println("Received a Ping for delivery");
      constructHeader(buffer, object);
    }
    else if (payload == Utility.PONG) 
    {
      // convert and send only to the original
      System.out.println("Received a Pong for delivery");
      if(constructHeader(buffer, object)) {
        offset += constructPongPayload(object);
      }
    }
    else if (payload == Utility.QUERY)
    {
      // convert and send to all but the original
      System.out.println("Received a Query for delivery");
      if(constructHeader(buffer, object)) {
        offset += constructQueryPayload(object);        
      }
    }
    else if (payload == Utility.QUERY_HIT)
    {
      // convert and send to only the original
      System.out.println("Received a QueryHit for delivery");
      if(constructHeader(buffer, object)) {
        offset += constructQueryHitPayload(object);
      }
    }
    else if (payload == Utility.PUSH) {

      // convert and ???
      System.out.println("Received a Push for delivery");
      if(constructHeader(buffer, object)) {
        offset += constructPushPayload(object);
      }
    }
    else 
    {
      System.out.println("Error delivering: cannot identify object type");
      return false;
    }
    // Send the message on to the socket, and report success
    if(send(buffer, offset, payload, socketId))
      return true;
    else {
      System.out.println("An unknown error occured in sending package.");
      return false;
    }
  }

  private static boolean constructHeader(byte[] buffer, MessageObject object) 
  {
    // place the header
    byte[] id = object.getMessageId();
    System.arraycopy(id, 0, buffer, 0, 16);
    
    // payload decriptor, ttl and hops 
    buffer[17] = (byte)object.getPayloadDescriptor();
    buffer[18] = (byte)object.getTtl();
    buffer[18] = (byte)object.getHops();

    // payload length
    int payloadLength = object.getPayloadLength();
    Utility.serializeInt(payloadLength, buffer, 19);

    return true;
  }



  private static int constructPongPayload(MessageObject object) {
    return 0;
  }
  private static int constructQueryPayload(MessageObject object) {
    return 0;
  }
  private static int constructQueryHitPayload(MessageObject object) {
    return 0;
  }
  private static int constructPushPayload(MessageObject object) {
    return 0;
  }

  private static boolean send(byte[] buffer, int offset, int payload, int socketId)
  {
    MessageServer.dispatch(buffer, offset, payload, socketId);
    return true;
  }
}
