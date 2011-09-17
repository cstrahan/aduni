import java.io.*;
import java.net.*;
// import gateway.Gateway;

public class MessageServer
{
  public static int port = 6346;
  public static int socketId = 0; 
  public static int MAXCONNECTIONS = 10;
  public static SocketHandler connection[];

  public static void main(String[] args)
  {
    System.out.println("*** This is the Footella server test program. ***");

    // Initialize the connection array
    connection = new SocketHandler[MAXCONNECTIONS];
    try
    {
      ServerSocket s = new ServerSocket(port);
      System.out.println("Server initialized and listening on port " + port);

      // Listen for new connections and spawn a new listener thread when it happens
      for(;;) {
        if(socketId <= MAXCONNECTIONS) {
          Socket incoming = s.accept();
          System.out.println("Received connection request, spawning thread " + socketId);
          connection[socketId] = new SocketHandler(incoming, socketId);
          connection[socketId].start();
          socketId++;
        }
        else
          System.out.println("Too many connections, cannot accept new connection.");
      }
    }
    catch(Exception e) {
      System.out.println("Server error: " + e);
    }
  }

  public static boolean dispatch(byte[] buffer, int offset, int payload, int socketId) {
    connection[socketId].send(buffer, offset);
    return true;
  }

  public static boolean getBytes(byte[] buffer, int length, int socketId)
  {
    connection[socketId].getBytes(buffer, length);
    return true;
  }
}   

class SocketHandler extends Thread {
  private static DataOutputStream oS;
  private static DataInputStream iS;

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
        oS = new DataOutputStream(incoming.getOutputStream());
        iS = new DataInputStream(incoming.getInputStream());
        byte[] inBuffer = new byte[1024];

        while(iS.read(inBuffer, 0, 23) != -1) {
        // Listen for commands
        Gateway.parseMessage(inBuffer, socketId);
        sleep(SLEEP_LENGTH);
        }
      }
      catch(Exception e)
      {
        System.out.println("Server section error occurred: " + e);
      }
    }
  }
   
  static boolean getBytes(byte[] buffer, int length) {
    try {
      iS.read(buffer, 0, length);
    }
    catch(Exception e) {
      System.out.println("Error reading payload");
      return false;
    }
    return true;
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

  public boolean send(byte[] buffer, int offset) 
  {
    System.out.println("Socket " + socketId + " received send request");
    try {
      oS.write(buffer, 0, offset);
      oS.flush();
      return true;
    }
    catch(Exception e) {
      System.out.println("SocketHandler " + socketId + ": Error sending message!");
      return false;
      }
  }

  Socket incoming;
  int socketId;
  private static int SLEEP_LENGTH = 100;
}

      
      
