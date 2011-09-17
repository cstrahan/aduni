/**
   Pong.java
   part of footella
   @version $Id: Pong.java,v 1.5 2001/01/28 15:58:50 jeff Exp $
*/
public class Pong extends MessageObject
{
    private int port;
    private byte[] ipAddress = new byte[4];
    private int sharedFiles;
    private int kilobytes;
    
  public Pong(int payloadDescriptor, int ttl, int payloadLength,
              int port, byte[] ipAddress, int sharedFiles, int kilobytes)
  {
    super(payloadDescriptor, ttl, payloadLength);
    this.port = port;
    this.ipAddress = ipAddress;
    this.sharedFiles = sharedFiles;
    this.kilobytes = kilobytes;
  }

  /**
     A full constructor
  */
  public Pong(byte[]messageId, int payloadDescriptor, int ttl, int hops,
              int payloadLength, int port, byte[] ipAddress, 
              int sharedFiles, int kilobytes)
  {
    super(messageId, payloadDescriptor, ttl, hops, payloadLength);
    this.port = port;
    this.ipAddress = ipAddress;
    this.sharedFiles = sharedFiles;
    this.kilobytes = kilobytes;
  }

  public int getPort()
  {
    return port;
  }

  public byte[] getIpAddress()
  {
    return ipAddress;
  }

  public int getSharedFiles()
  {
    return sharedFiles;
  }

  public int getKilobytes()
  {
    return kilobytes;
  }
  public String toString() {
    String aString =  /* super.toString() + */
      "PONG [ port " + port + " | ip " +
      ((int)ipAddress[0] &0xff) + "." + 
      ((int)ipAddress[1] &0xff) + "." +
      ((int)ipAddress[2] &0xff) + "." + 
      ((int)ipAddress[3] &0xff) + 
      " | " + sharedFiles + " files | " + kilobytes + " kB ]";
    return aString;
  }

  /**
     A test main routine
  */
  public static void main(String[] args) {
    System.out.println("This is Pong");
    byte[] ipAddress = { 127, 0, 0, 1};
    Pong testPong = new Pong(1, 5, 14, 6346, ipAddress,
                             4, 14920);
    System.out.println("Pong made, testing ...");
    System.out.println(testPong);
  }
}






