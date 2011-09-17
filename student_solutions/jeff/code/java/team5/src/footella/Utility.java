/**
   MessageServer.java
   part of footella

   @author JMR
   @version $Id: Utility.java,v 1.16 2001/01/31 16:10:49 jeff Exp $

   A static class which takes 
*/

import java.net.*;


public class Utility
{
  public static int currentIncoming = 0;
  public static int currentOutgoing = 0;
  public static int MAX_INCOMING = 1;
  public static int MAX_OUTGOING = 2;

  private static int numFiles = 0;
  private static int shareSize = 0;
  public static final int DOWNLOAD = 0;
  public static final byte PING = (byte)0x00;
  public static final byte PONG = (byte)0x01;
  public static final byte QUERY = (byte)0x80;
  public static final byte QUERY_HIT = (byte)0x81;
  public static final byte PUSH = 0x40;
  public static InetAddress ipAddress;
  private static int portNum = 6346;
  public static byte[] ip = new byte[4];
  private static int ttl = 7;
  public static String downloadPath = ".";
  public static String searchPath = ".";

  public static void setTtl(int t) {
    ttl = t;
  }
  public static void setDownloadPath(String d) {
    downloadPath = d;
  }
  public static void setSearchPath(String s) {
    searchPath = s;
  }

  public static byte[] getIp() {
    byte[] temp = new byte[4];
    System.arraycopy(ip, 0, temp, 0 , 4);
    return temp;
  }
    
  public static int getNumFiles() {
    return numFiles;
  }
  public static void setNumFiles(int n) {
    numFiles = n;
  }

  public static int getSharedSize() {
    return shareSize;
  }
  public static void setSharedSize(int n) {
    shareSize = n;
  }

    public static byte[] getServentId() {
	String temp = "GARYJEFFTONYERIC";
	byte[] foo = temp.getBytes();
	return foo;
    }

  public static String getAddress()
  {
    return ipAddress.getHostAddress();
  }

  public static int getPort() {
    return portNum;
  }

  public static void setPort(int port) {
    portNum = port;
  }
  
  public static int getTtlPref()
  {
    return ttl;
  }

  public static void setIpAddress(InetAddress ipA)
  {
    ipAddress = ipA;
    byte[] temp = ipAddress.getAddress();
    System.arraycopy(temp, 0, ip, 0, 4);
  }

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

    return offset;
  }

  public static int serializePort(int value, byte[] outBuffer, int offset)
  {
    outBuffer[offset++] = (byte)(value);
    outBuffer[offset++] = (byte)(value >> 8);
    return offset;
  }

  public static int deserializePort(byte[] inBuffer, int offset)
  {
    return (inBuffer[offset + 1] &0xff) << 8  |
      (inBuffer[offset]    &0xff);
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

  public static String grabString(byte[] buffer, int offset)
  {
    //    System.out.println("Entered Grab String");
    String temp = "";
    while(true) 
    {
      if(offset == 1024) {
        // unrealistic results, dump. Perhaps too paranoid
        // System.out.println("QueryHit: Cannot parse payload!");
        return ""; // error check
      }
      char b = (char)buffer[offset];
      if(b == 0x00)
        if((char)buffer[offset + 1] == 0x00) {
          offset += 2;
          return temp;
        }
      temp += b;
      offset++;
    }
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

