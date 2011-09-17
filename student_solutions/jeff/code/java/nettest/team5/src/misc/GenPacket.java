class GenPacket
{
  /**
     This method generates an unique messageId(hopefully)
     @return The id, 16 bytes long
  */
  private static byte[] generateMessageID() {
    byte[] temp = new byte[16];
    // Implement this! 
    for(int i = 0; i < 16; i++) temp[i] = 0; // for now
    return  temp;
  }
}
