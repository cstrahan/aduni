/**
   PingHandler.java
   part of footella
   @author Erica K.
   @version $Id: PingHandler.java,v 1.13 2001/01/28 16:33:22 jeff Exp $
*/
public class PingHandler {
  Ping ping;
  
  /**
     Constructor for handling outgoing pings
  */
  public PingHandler(Gateway gateway) {
    //arbitrarily chosen timetolive
    int ttl = 10;
    ping = new Ping(0, ttl, 0);
  }
    
  /**
     Constructor for handling incoming pings
  */
  public PingHandler(Ping incomingPing) {
    ping = incomingPing;
  }

  /**
     Receives a ping, and acts appropriately
     @param incomingPing the Ping object
  */
  public static void receivePing(Ping incomingPing) {
    // propogatePing(incomingPing);
    // send a Pong!
    sendPong(incomingPing);
  }

  /**
     For initiating pings
  */
  public static void sendPing() {
    Ping ping = new Ping(0, 10, 0);
    Gateway.deliver(ping);
  }

  /**
     Sends a ping on a specific socket
  */
  public static void sendPing(int socketId) {
    Ping ping = new Ping(0, 10, 0);
    ping.setSocketId(socketId);
    Gateway.deliver(ping);
  }

  /**
     For sending pings on
  */
  public static void propagatePing(Ping ping) {
    ping.decTtl();
    ping.incHops();

    int ttl = ping.getTtl();
    if (ttl > 0)
    {
      Gateway.deliver(ping);
    }
  }

  public static void sendPong(Ping ping) {
    byte[] messageId = ping.getMessageId();
    int pongDescriptor = 1;
    int ttl = Utility.getTtlPref();
    int hops = 0;
    int payloadLength = 14;
    int port = Utility.getPort();
    byte[] ipAddress = Utility.getIp();
    int sharedFiles = 100; // grab this from Utility
    int kilobytes = 1000; // grab this from Utility
	
    Pong pong = new Pong(messageId, pongDescriptor, ttl, hops, payloadLength,
                         port, ipAddress, sharedFiles, kilobytes);
    pong.setSocketId(ping.getSocketId());
    Gateway.deliver(pong);
  }
}






