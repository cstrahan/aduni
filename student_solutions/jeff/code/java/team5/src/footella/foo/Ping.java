/**
   Ping.java
   @version $Id: Ping.java,v 1.4 2001/01/26 17:02:31 jeff Exp $
*/
public class Ping extends MessageObject
{
  Ping()
  {
    super(0, 7, 0);
  }
  
  public Ping(int payloadDescriptor, int ttl, int payloadLength)
  {
    super(payloadDescriptor, ttl, payloadLength);
  }
  public Ping(byte[] messageId, int payloadDescriptor,
                int ttl, int hops, int payloadLength)
  {
    super(messageId, payloadDescriptor, ttl, hops, payloadLength);
  }
  public String toString()
  {
    return "PING";
  }
}
