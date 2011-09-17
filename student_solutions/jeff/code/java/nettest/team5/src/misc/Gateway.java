/**
   Gateway.java

   The gateway is responsible for making sure messages are properly
   and securely parsed. When it is convinced the packet and its 
   contents are good, it will create an object of the appropriate type
   and send it to the appropriate place.
*/   

public class Gateway
{
  /**
     Right now, this just reads information about an incoming 
     descriptor header. No security (yet)
  */
  public static void parseDescriptorHeader(byte[] header) 
  {
    System.out.println("Attempting to parse header.");
    byte[] messageId = new byte[16];
    try {
      // Grab the messageId
      System.arraycopy(header, 0, messageId, 0, 16);

      // Check it with the table, and add if it necessary
      // *********** CODE GOES HERE **************
    }
    catch(Exception e)
    { 
      System.out.println("parseDescriptorHeader(): Bad messageId");
      // *********** CODE GOES HERE **************
    } 
    // Try to parse the meaning of the payload
    int payloadDescriptor = header[16];

    if(payloadDescriptor == 0x40)
      System.out.println(" Push anticipated");
    else if(payloadDescriptor == 0x00)
      System.out.println(" Ping anticipated");
    else if(payloadDescriptor == 0x01)
      System.out.println(" Pong anticipated");
    else if(payloadDescriptor == 0x80)
      System.out.println(" Query anticipated");
    else if(payloadDescriptor == 0x81)
      System.out.println(" QueryHit anticipated");
    else 
    {
      System.out.println(" Unknown, bailing out.");
      // ****************** BAIL OUT HERE ********************
    }
    
    // Grab ttl
    int ttl = header[17];
    
    // ***************** CHECK TTL RANGE **********************

    // Grab hops
    int hops = header[18];

    // ****************** CHECK HOPS AGAINST TTL **************

    // Grab payload
    int payloadLength = Utility.deserializeInt(header, 19);
    System.out.println(" Expecting payload of " + payloadLength);
    
    /*
      At this point, the descriptor checks out, and we're good to go. Now
      to create the appropriate object and send it to the right handler
    */

    // For now... 
    MessageObject temp = new MessageObject(messageId, payloadDescriptor, 
                                           ttl, hops, payloadLength);

    System.out.print("Descriptor header successfully parsed!");
    if(payloadLength > 0)
      System.out.println("Attempting to extract payload...");
    else
      System.out.println();
      
    System.out.println(temp);
  }

  private static parsePayload(byte[] payloadContents,
                              int payloadDescriptor,
                              int payloadLength)
  {
    switch(payloadLength) 
    {
      case 1:                   // query
        // do something
        buildPing() {}
        break;
      case 2:                   // query hit
        // do something
        break;
      case 3:                   // push
        // do something
        break;
      default:                  // unknown
        System.out.println("extractPayload(): Cannot identify payload");
        // ************* BAIL OUT ******************
        break;
    }
  }

  public static void buildPing() {}
  public static void buildPong() {}
  public static void buildQuery() {}
  public static void buildQueryHit() {}
  public static void buildPus() {}

  public static boolean deliver(MessageObject object) { return true; }
  
}
