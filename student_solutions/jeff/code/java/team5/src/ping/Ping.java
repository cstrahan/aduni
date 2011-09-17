/**
   Ping.java
   @version $Id: Ping.java,v 1.3 2001/01/23 16:49:54 eklempne Exp $
*/
public class Ping extends MessageObject
{
  public Ping(int payloadDescriptor, int ttl, int payloadLength)
  {
    super(payloadDescriptor, ttl, payloadLength);
  }
  public Ping(byte[] messageId, int payloadDescriptor,
                int ttl, int hops, int payloadLength)
  {
    super(messageId, payloadDescriptor, ttl, hops, payloadLength);
  }
}







