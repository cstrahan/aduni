import java.io.*;
import java.net.*;

public class ClientTest {
  public static void main(String[] args) { 
    System.out.println("*** This is the Footella client test program. ***");
    try {
      // Initialize variables
      int port = 8189;
      String domain = "localhost";
      Socket s = new Socket(domain, port);
      BufferedReader inStream = new BufferedReader(new InputStreamReader(s.getInputStream()));
      Writer outStream = new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));

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
      if(handshakeOut.equals(foo))
        System.out.println("I have successfully shaken hands with the server."); // Woot!
      else
        System.out.println("Cannot connect to " + domain + ":" + port); // No dice

      // Clean up
      outStream.close();
      inStream.close();
      s.close();

    }
    catch(ConnectException e) {
      System.out.println("Cannot connect to server");
    }
    catch(IOException e) {
      System.out.println("An Error occured: " + e);
      System.exit(1);
    } 
  }
}

    
                                             
      
        
  
