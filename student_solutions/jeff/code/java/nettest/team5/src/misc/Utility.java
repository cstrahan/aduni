public class Utility
{
  /**
     Takes an integer and turns it into an byte array
     @param value The value to be serialized
     @param outbufffer The byte buffer
     @param offset Starting place in the buffer
     @return The new Offset
  */
  public static int serializeInt(int value, byte[] outBuffer, int offset)
  {
    outBuffer[offset++] = (byte)(value);
    outBuffer[offset++] = (byte)(value >> 8);
    outBuffer[offset++] = (byte)(value >> 16);
    outBuffer[offset++] = (byte)(value >> 24);

    // return offset
    return offset;
  }

  /**
     Converts a series of bytes into an integer value
     @param inBuffer The byte buffer
     @param offset The starting place in the buffer
     @return The integer value converted from byte[offset ... offset + 3]
  */
  public static int deserializeInt(byte[] inBuffer, int offset)
  {
    return (inBuffer[offset + 3]) << 24 |
      (inBuffer[offset + 2] &0xff) << 16 |
      (inBuffer[offset + 1] &0xff) << 8  |
      (inBuffer[offset]    &0xff);
  }

  /** Small test of functions */
  public static void main(String[] args)
  {
    int testInt = 12458;
    System.out.println("Sending in int " + testInt);

    byte b[] = new byte[4];
    System.out.println("Converting to byte[4]");
    serializeInt(testInt, b, 0);
    System.out.print("Byte value of our int is: ");
    for(int i = 0; i < 4; i++)
      System.out.print("0x" + Integer.toHexString(b[i]) + " ");
    System.out.println("\nWhich deserializes to " + deserializeInt(b, 0));
  }
}

