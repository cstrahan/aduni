/**
   PingHandler.java
   @version $Id: PingHandler.java,v 1.6 2001/01/24 11:56:16 eklempne Exp $
*/
public class PingHandler
{
    /**
       Called by Gateway when incoming ping received
    */
    public static void receivePing(Ping ping)
    {
	propagatePing(ping);
	sendPong(ping);
    }
    
  /**
     For sending pings on
  */
  public static void propagatePing(Ping ping)
  {
    ping.decTtl();
    ping.incHops();

    int ttl = ping.getTtl();
    if (ttl > 0) Gateway.deliver(ping);

  }

    /**
       Response to a ping
    */
  public static void sendPong(Ping ping)
  {
      byte[] messageId = new byte[16];
      messageId = ping.getMessageId();
      int payloadDescriptor = 1;
    int ttl = 10; // ?
    int hops = 0;
    int payloadLength = 14;
    int port = 6346;
    byte[] ipAddress = {0, 0, 0, 0}; // no idea right now
    int sharedFiles = numberOfFiles();
    int kilobytes = numberOfKilobytes();
	
    Pong pong = new Pong(messageId, payloadDescriptor, ttl, hops, payloadLength,
                         port, ipAddress, sharedFiles, kilobytes);
    pong.setSocketId(ping.getSocketId());
    Gateway.deliver(pong);
  }

  /**
     Method to find number of shared files...?
  */
  public static int numberOfFiles()
  {
    return 0;
  }

  /**
     Method to find number of shared kilobytes...?
  */
  public static int numberOfKilobytes()
  {
    return 0;
  }

  /**
     For initiating pings - (called by...? MessageServer? Controller?)
  */
  public static void initiatePing()
  {
      int ttl = 10; // say
      int socketId = 0; // ??? - want to ping all connected hosts; don't
      // yet see how to get socketId's for this
      Ping ping = new Ping(0, ttl, 0);
      ping.setSocketId(socketId);
      
      Gateway.deliver(ping);
  }

}






