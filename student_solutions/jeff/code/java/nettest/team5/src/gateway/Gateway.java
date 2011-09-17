public class Gateway
{
  /**
     Right now, this just reads information about an incoming 
     descriptor header. No security right now.
  */
  public static void parseDescriptorHeader(byte[] header) 
  {
    byte[] messageId = new byte[16];
    try {
      System.arraycopy(header, 0, messageId, 0, 16);
    }
    catch(Exception e)
    { 
      System.out.println("parseDescriptorHeader(): Bad messageId");
    } 
    // Try to parse the meaning of the payload
    int payloadDescriptor = header[16];
    if(payloadDescriptor == 0x40)
      System.out.println(" Push");
    else if(payloadDescriptor == 0x00)
      System.out.println(" Ping");
    else if(payloadDescriptor == 0x01)
      System.out.println(" Pong");
    else if(payloadDescriptor == 0x80)
      System.out.println(" Query");
    else if(payloadDescriptor == 0x81)
      System.out.println(" QueryHit");
    else
      System.out.println(" Unknown");

    // Grab ttl
    int ttl = header[17];
    // Grab hops
    int hops = header[18];
    // Grab payload
    int payloadLength = deserializeInt(header, 19);

    /*
    if(payloadLength > 0) 
    {
      byte[] payloadContents = new byte[payloadLength];
      extractPayload(payloadContents, payloadDescriptor, payloadLength);
    }
    */
  }

  private static void extractPayload(byte[] payloadContents,
                              int payloadDescriptor,
                              int payloadLength)
  {
    switch(payloadLength) 
    {
      case 1:                   // query
        // do something
        break;
      case 2:                   // query hit
        // do something
        break;
      case 3:                   // push
        // do something
        break;
      default:                  // unknown
        System.out.println("extractPayload(): Cannot identify payload");
        break;
    }
  }
  public static int deserializeInt(byte[] inBuffer, int offset)
  {
    return (inBuffer[offset + 3]) << 24 |
      (inBuffer[offset + 2] &0xff) << 16 |
      (inBuffer[offset + 1] &0xff) << 8  |
      (inBuffer[offset]    &0xff);
  }
}
