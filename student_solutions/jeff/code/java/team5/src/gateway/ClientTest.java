import java.io.*;
import java.net.*;

/**
   A _very_ rudimentary gnutella client program
   @author JMR
   @version $Id: ClientTest.java,v 1.1 2001/01/23 19:08:58 jeff Exp $
*/
public class ClientTest {
  public static void main(String[] args) { 
    System.out.println("*** This is the Footella client test program. ***");
    try {
      // Initialize variables
      int port = 6346;
      String domain = "localhost";
      Socket s = new Socket(domain, port);

      if(handshake(s)) {
        // Do stuff
        while(true)
        {
          DataOutputStream oS = new DataOutputStream(s.getOutputStream());
          DataInputStream iS = new DataInputStream(s.getInputStream());

          byte[] buffer = new byte[23];


          // Send some dummy messages to the server
          for(int i = 0;i < 1000; i++) 
          {
            makeHeader(buffer);
            System.out.println("Sending dummy message " + i);
            // Send buffer
            oS.write(buffer, 0, buffer.length);
            oS.flush();
          }
          break;
        }

        // Clean up
        //          oS.close();
        //          iS.close();
        s.close();
      }
    }
    catch(ConnectException e) {
      System.out.println("Cannot connect to server");
    }
    catch(IOException e) {
      System.out.println("An Error occured: " + e);
      System.exit(1);
    } 
  }

  static boolean handshake(Socket s) {
    try {
      BufferedReader inStream = 
        new BufferedReader(new InputStreamReader(s.getInputStream()));
      Writer outStream = 
        new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));

      String message = "GNUTELLA CONNECT/0.4\n\n";
      String handshakeOut = "GNUTELLA OK\n";

      // Try to send a message
      outStream.write(message, 0, message.length());
      outStream.flush();

      // Try to read the response
      char test[] = new char[12];
      inStream.read(test, 0, 12);

      // Check to see what it is
      String foo = String.copyValueOf(test);
      if(handshakeOut.equals(foo)) {
        System.out.println("I have successfully shaken hands with the server."); // Woot!
        return true;
      }
      else {
        System.out.println("Cannot connect."); // No dice
        return false;
      }
    }
    catch(Exception e) {
      System.out.println("Error in handshaking.");
      return false;
    }
  }
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
}    
                                             
      
        
  
