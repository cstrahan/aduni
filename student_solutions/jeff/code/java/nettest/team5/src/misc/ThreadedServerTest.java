import java.io.*;
import java.net.*;
// import footella.Utility;

public class ThreadedServerTest
{


  public static void main(String[] args)
  {
    System.out.println("*** This is the Footella server test program. ***");
    int port = 6346;
    int socketId = 1; 
    try
    {
      ServerSocket s = new ServerSocket(port);
      System.out.println("Server initialized and listening on port " + port);

      // Listen for new connections and spawn a new listener thread when it happens
      for(;;) {
        Socket incoming = s.accept();
        socketId++;
        System.out.println("Received connection request, spawning thread " + socketId);
        new SocketHandler(incoming, socketId).start();
      }
    }
    catch(Exception e) {
      System.out.println("Server error: " + e);
    }
  }
}   


class SocketHandler extends Thread {
  /** Constructor */
  public SocketHandler(Socket i, int c)
  {
    incoming = i;
    socketId = c;
  }
  /**
     Run a thread
  */
  public void run()
  {
    if(handshake(incoming)) {
      try {
        DataOutputStream oS = new DataOutputStream(incoming.getOutputStream());
        DataInputStream iS = new DataInputStream(incoming.getInputStream());
        byte[] inBuffer = new byte[1024];

        while(iS.read(inBuffer, 0, 23) != -1) {
        // Listen for commands
        Gateway.parseDescriptorHeader(inBuffer);
        sleep(SLEEP_LENGTH);
        }
      }
      catch(Exception e)
      {
        System.out.println("Server section error occurred: " + e);
      }
    }
  }
   

  static boolean handshake(Socket incoming) {
    try {
      BufferedReader inStream;
      Writer outStream; 
      String message = "GNUTELLA OK\n";
      String handshakeIn = "GNUTELLA CONNECT/0.4\n\n";
      char test[] = new char[22];
      
      // Wait for a message...
      inStream = new BufferedReader(new InputStreamReader(incoming.getInputStream()));
      outStream = new BufferedWriter(new OutputStreamWriter(incoming.getOutputStream()));
      System.out.println("Connection established.");
      inStream.read(test, 0, 22);

      String foo = String.copyValueOf(test);
      if(handshakeIn.equals(foo)) {
        System.out.println("Authorized connection.");
        outStream.write(message, 0, message.length());
        outStream.flush();
        return true;
      }
      else return false;
    }
    catch(Exception e) {
      System.out.println("Server error: " + e);
      return false;
    }
  }

  /* */
  public int getSocketId() { return socketId; } 

  Socket incoming;
  int socketId = 0;
  private static int SLEEP_LENGTH = 100;
}

      
      
