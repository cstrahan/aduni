class TestByte
{
  public static void makeHeader(byte[] buffer)
  {
    // Just a boring packet, for testing.
    createMessageId(buffer);
    buffer[16] =  0x40;
    buffer[17] = 4;
    buffer[18] = 2;
    Utility.serializeInt(1492, buffer, 19);
  }

  public static void createMessageId(byte[] buffer)
  {
    for(int i = 0; i < 16; i++)
      buffer[i] = 0;
    String temp = "adu10.11.0.65" + counter++;
    byte[] tempByte = temp.getBytes();
    if(tempByte.length < 16)
      System.arraycopy(tempByte, 0, buffer, 0, tempByte.length);
    else
      System.arraycopy(tempByte, 0, buffer, 0, 16);
  }

  public static void parseDescriptorHeader(byte[] header) 
  {
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
  public static int counter = 1;
}

