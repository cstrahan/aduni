public class BytePackTest
{
  static int counter = 1;
  
  public static void main(String[] args)
  {
    // set up a test byte
    byte[] test = new byte[23];
    for(int i = 0; i < 23 ; i++)
      test[i] = 0x00;
    test[16] = 0x00;
    test[22] = 127;
    
    System.out.println();

    byte[] temp = createMessageId();
    System.arraycopy(temp, 0, test, 0, 16);
    parseDescriptorHeader(test);
  }

  public static byte[] createMessageId()
  {
    byte[] msgId = new byte[16];
    for(int i = 0; i < 16; i++)
      msgId[i] = 0;
    String temp = "adu10.11.0.65" + counter++;
    byte[] tempByte = temp.getBytes();
    if(tempByte.length < 16)
      System.arraycopy(tempByte, 0, msgId, 0, tempByte.length);
    else
      System.arraycopy(tempByte, 0, msgId, 0, 16);

    // What is the best way to convert an integer to a fixed byte length?
    return msgId;
  }
  

  public static void parseDescriptorHeader(byte[] header) 
  {
    for(int i = 0; i < 23 ; i++)
      System.out.print("0x" +Integer.toHexString(header[i]) + " ");

    System.out.println();
    System.out.print("\nMessage ID:         ");
    for(int i = 0; i < 16 ; i++)
      System.out.print((char)header[i]);

    // Try to parse the meaning of the payload
    System.out.print("\nPayload Descriptor: 0x" + Integer.toHexString(header[16]));
    if(header[16] == 0x40)
      System.out.println(" Push");
    else if(header[16] == 0x00)
      System.out.println(" Ping");
    else if(header[16] == 0x01)
      System.out.println(" Pong");
    else if(header[16] == 0x80)
      System.out.println(" Query");
    else if(header[16] == 0x81)
      System.out.println(" QueryHit");
    else
      System.out.println(" Unknown");
    
    System.out.println("TTL:                " + header[17]);
    System.out.println("Hops:               " + header[18]);
    System.out.print("Payload Length:     ");
    int getLength = 0;
    for(int i = 18; i < 23; i++) 
      getLength += header[i];
    System.out.println(getLength + "\n");
  }
}

