/**
   PushHandler.java
   part of footella
   @version $Id: PushHandler.java,v 1.3 2001/01/24 16:10:25 jeff Exp $
*/
public class PushHandler
{
    // what do we want to do with this - connect and upload...?
    public static void receivePush(Push push) {

    }

  /*
    Commented out right now for testing. Looks good!
  */
//    public static void sendPush(QueryHitObject qho, int fileIndex)
//    {
//      int payloadDescriptor = 0x40;
//      // probably access this information from a default settings file (eventually)
//      int ttl = 10; // ???
//      int payloadLength = 26;
	
//      byte[] serventID = new byte[16];
//      serventID = qho.getServentID();
//      byte[] ipAddress = new byte[4];
//      ipAddress = qho.getIpAddress();
//      int port = qho.getPort();

//      Push push = new Push(payloadDescriptor, ttl, payloadLength,
//                           serventID, fileIndex, ipAddress, port);
//      Gateway.deliver(push);
//    }

}







