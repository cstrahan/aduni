/**
   ClientTest.java
   part of footella
   A _very_ rudimentary gnutella client program
   @author JMR
   @version $Id: ClientTest.java,v 1.6 2001/01/25 18:07:51 jeff Exp $
*/

import java.io.*;
import java.net.*;

public class ClientTest {
  public static void main(String[] args) { 
    ClientThread myClient = new ClientThread();
    myClient.start();
  }
}

class ClientThread extends Thread 
{
  public void run() 
  {
          
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

          // Send some dummy messages to the server
          for(int i = 0;i < 10; i++) 
          {
            byte[] buffer = new byte[1024];
            int offset = sendPackage(buffer);
            // Send buffer
            System.out.println("Client: sending message of " + offset + " bytes");
            oS.write(buffer, 0, offset);
            oS.flush();
          }

          // Clean up
          //          oS.close();
          //          iS.close();
          //        s.close();
      
          byte[] inBuffer = new byte[1024];
          do {
            // Listen for commands
            System.out.println("\nGOT A PONG!");
            //            Gateway.parseMessage(inBuffer, 0);
            sleep(100);
          } while(iS.read(inBuffer, 0, 37) != -1);
          s.close();
        }
      }
    }
    
    catch(ConnectException e) {
      System.out.println("Cannot connect to server");
    }
    catch(IOException e) {
      System.out.println("An Error occured: " + e);
      System.exit(1);
    } 


    catch(Exception e)
    {
      System.out.println("Server section error occurred: " + e);
    }
  }

  static boolean handshake(Socket s) {
    try {
      BufferedReader inStream = 
        new BufferedReader(new InputStreamReader(s.getInputStream()));
      Writer outStream = 
        new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));

      String message = "GNUTELLA CONNECT/0.4\n\n";
      String handshakeOut = "GNUTELLA OK\n\n";

      // Try to send a message
      outStream.write(message, 0, message.length());
      outStream.flush();

      // Try to read the response
      char test[] = new char[13];
      inStream.read(test, 0, 13);

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
  public static int sendPackage(byte[] buffer)
  {
    double foo = (double)Math.random();
    if(foo >= .75) 
    {
      // a Query for testing.
      createMessageId(buffer);
      buffer[16] = (byte)0x80;
      buffer[18] = 2;
      Utility.serializeInt(13, buffer, 19);
      Utility.serializePort(6346, buffer, 23);
      String search = "foo bar baz";
      byte[] baz = search.getBytes();
      System.arraycopy(baz, 0, buffer, 25, search.length());

      return 23 + 2 + 11;
    }
    else if(foo >= .50)
    {
      // a Pong for testing.
      createMessageId(buffer);
      buffer[16] =  0x01;
      buffer[17] = 4;
      buffer[18] = 2;
      Utility.serializeInt(14, buffer, 19);
      Utility.serializePort(6346, buffer, 23);
      buffer[25] = 127;
      buffer[26] = 0;
      buffer[27] = 0;
      buffer[28] = 1;
      Utility.serializeInt(4, buffer, 29);
      Utility.serializeInt(14920, buffer, 33);

      System.out.print("Client trying to send pong: " );
      for(int i = 23; i < 36; i++)
        System.out.print(buffer[i]);
      System.out.println();
      return 37;
    }

    else if(foo >= .25)
    {
      // a QueryHit for testing
      createMessageId(buffer);
      buffer[16] = (byte)0x81;
      System.out.println(buffer[16]);
      buffer[17] = 3;
      buffer[18] = 4;
      buffer[23] = 1;
      Utility.serializePort(6364, buffer, 24);
      buffer[26] = 10;
      buffer[27] = 11;
      buffer[28] = 0;
      buffer[29] = 65;
      Utility.serializeInt(28800, buffer, 30);
      Utility.serializeInt(1492, buffer, 34);
      Utility.serializeInt(5678, buffer, 38);
      char[] temp = 
      {
        'F' , 'o' ,'o', '.' , 'm' , 'p' , '3' , '\n' , '\n'
      };

      for(int i = 0; i < temp.length; i++)
        buffer[i + 42] = (byte)temp[i];

      for(int i = 0; i < 16; i++)
        buffer[i + 51] = 0;
      String temp2 = "124869245baz" + counter++;
      byte[] tempByte = temp2.getBytes();
      if(tempByte.length < 16)
        System.arraycopy(tempByte, 0, buffer, 51, tempByte.length);
      else
        System.arraycopy(tempByte, 0, buffer, 51, 16);


      Utility.serializeInt(44, buffer, 19);

      return 67;
    }
    else 
    {
      // a Ping for testing.
      createMessageId(buffer);
      buffer[16] = 0x00;
      buffer[17] = 4;
      buffer[18] = 2;
      Utility.serializeInt(0, buffer, 19);
      System.out.println("Client trying to send ping: " );
      return 23;
    }
  }

  public static Pong sendPong()
  {
    byte[] ipAddress = { 127, 0, 0, 1};
    Pong testPong = new Pong(1, 5, 14, 6346, ipAddress,
                             4, 14920);
    return testPong;
  }

  public static void createMessageId(byte[] buffer)
  {
    for(int i = 0; i < 16; i++)
      buffer[i] = 0;
    String temp = "foo114.24.6.78" + counter++;
    byte[] tempByte = temp.getBytes();
    if(tempByte.length < 16)
      System.arraycopy(tempByte, 0, buffer, 0, tempByte.length);
    else
      System.arraycopy(tempByte, 0, buffer, 0, 16);
  }
  private static int counter = 0;
}    
                                             
      
        
  
