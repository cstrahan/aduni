import java.io.*;
import java.net.*;

public class PingTest {
  public static void main(String[] args) { 
    System.out.println("I am a Client. Woohoo.");
    try {
      Socket s = new Socket("localhost", 8189);
      BufferedReader inStream = new BufferedReader(new InputStreamReader(s.getInputStream()));
      Writer outStream = new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));

      String message = "GNUTELLA CONNECT/0.4\n\n";

      outStream.write(message, 0, message.length());
      outStream.flush();
      
      char test[] = new char[1024];
      inStream.read(test, 0, 13);

      String handshakeOut = "GNUTELLA OK\n";
      String foo = String.copyValueOf(test);
      System.out.print(foo);
      
      if(handshakeOut.equals(foo))
        System.out.println("I have successfully shaken hands with the server.");


      outStream.close();
      inStream.close();
      s.close();

    }
    catch(IOException e) {
      System.out.println("An Error occured: " + e);
      System.exit(1);
    } 
  }
}

    
                                             
      
        
  
