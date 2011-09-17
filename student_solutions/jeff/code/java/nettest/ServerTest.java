import java.io.*;
import java.net.*;
import footella.Utility;

public class ServerTest
{
  public static void main(String[] args)
  {
    System.out.println("*** This is the Footella server test program. ***");
    int port = 8189;
    
    try
    {
      ServerSocket s = new ServerSocket(port);
      System.out.println("Server initialized and listening on port " + port);
      BufferedReader inStream;

      Writer outStream; 
      String message = "GNUTELLA OK\n";
      String handshakeIn = "GNUTELLA CONNECT/0.4\n\n";
      char test[] = new char[22];
      
      // Wait for a message...
      Socket incoming = s.accept();
      inStream = new BufferedReader(new InputStreamReader(incoming.getInputStream()));
      outStream = new BufferedWriter(new OutputStreamWriter(incoming.getOutputStream()));
      System.out.println("Connection established.");
      inStream.read(test, 0, 22);

      String foo = String.copyValueOf(test);
      if(handshakeIn.equals(foo))
      {
        System.out.println("Authorized connection.");
        outStream.write(message, 0, message.length());
        outStream.flush();
      }
      DataOutputStream oS = new DataOutputStream(incoming.getOutputStream());
      DataInputStream iS = new DataInputStream(incoming.getInputStream());

      runTest(iS, oS);
    }

    catch(Exception e)
    {
      System.out.println("Server section error occurred: " + e);
    }
  }
    public static void runTest(DataInputStream iS, DataOutputStream oS) 
  {
    System.out.println("Trying to read a magic number...");
    byte[] foobar = new byte[4];
    try
    {
      iS.read(foobar, 0, 4);
      int magicNum = Utility.deserializeInt(foobar, 0);
      System.out.println(magicNum);
    }
    catch(Exception e)
    {
      System.out.println("runTest() error:  " + e);
    }
  }
}

      
      
