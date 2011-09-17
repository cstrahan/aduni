import java.io.*;
import java.net.*;

/**
   SocketHandler.java
   part of footella
   @author JMR
   @version $Id $

   This class contains a socket, as well as
   some other useful information, such as 
   a unique identifier, and information about
   messages sent and received.
*/

class SocketHandler extends Thread {

  /** Constructor */
  SocketHandler(Socket i, int socketId, InetAddress inet,
                ControlHandler control) {
    incoming = i;
    this.socketId = socketId;
    this.inet = inet;
    this.control = control;
    downloadHandler = control.getDownloadHandler();
  }

  /**
     Run listens for new messages
  */
  public void run() {
    byte[] buffer = new byte[1024];

    for(;;) {
      try {
        iS = new BufferedInputStream(incoming.getInputStream());
        while(iS.read(buffer, 0, 23) != -1) {

          // Otherwise, assume it's a packet, and send it off
          Gateway.parseMessage(buffer, this);
          rec++;
          
          sleep(SLEEP_LENGTH);
        }
      }
      catch(Exception e) {
        control.reportErr(new String("SocketHandler error occurred: " + e));
        autodisconnect();
      }
    }
  }

  /* accessors and mutators */
  public String getAddress() { return inet.getHostAddress(); }
  public int getSent() { return sent; }
  public int getRec() { return rec; }
  public int getSocketId() { return socketId; } 
  public int getHostPort() { return incoming.getPort(); }

  /**
     Disconnects the socket, usually called internally on an error
     @return true on success
  */
  public boolean autodisconnect() {
    try {
        live = false;
        control.removeConnection(socketId);
        incoming.close(); 
        this.stop();
    }
    catch(Exception e) { 
      control.reportErr(new String("Error closing socket " + socketId));
      this.stop();
      return true;
    }
    control.report(new String("Socket closed"));
    return true;
  }

  /**
     Disconnects the socket, called externally
     @return true on success
  */
  public boolean disconnect() {
    try {
      live = false;
      iS.close();
      oS.close();
      incoming.close(); 
      //   this.stop();
    }
    catch(Exception e) { 
      control.reportErr(new String("Error closing socket " + socketId));
      this.stop();
      return true;
    }
    control.report(new String("Socket closed"));
    return true;
  }

  /**
     Reads a number of bytes from the input stream
     @param buffer The write target
     @param length The number of bytes to read
  */
  public boolean getBytes(byte[] buffer, int length) {
    try { 
      iS.read(buffer, 0, length); 
    }
    catch(Exception e) {
      control.reportErr(new String("Error reading payload"));
      control.removeConnection(socketId);
      return false;
    }
    return true;
  }

  /**
     Attempts to negotiate a connection.
     @param incoming The other socket. ;)
     @return true on success
  */
  boolean handshake(Socket incoming) {
    try {
      BufferedReader inStream;
      Writer outStream; 
      String message = "GNUTELLA OK\n\n";
      String handshakeIn = "GNUTELLA CONNECT/0.4\n\n";
      char test[] = new char[22];
      
      // Wait for a message...
      inStream = new BufferedReader(new InputStreamReader(incoming.getInputStream()));
      outStream = new BufferedWriter(new OutputStreamWriter(incoming.getOutputStream()));

      // check to see final there is a get
      inStream.read(test, 0, 3);
      String foo = String.copyValueOf(test, 0 , 3);
      if(foo.equals("GET")) {
        System.out.println("GET request");
        downloadHandler.receiveDownloadRequest(incoming, inStream, outStream);
        return false;
      }

      // Not a get. Now check to see if we can connect any more
        if(Utility.currentIncoming >= Utility.MAX_INCOMING) {
          // too many connections
          control.report(new String("Denying incoming connection. Maximum incoming connections " +
                                    " engaged"));
          incoming.close();
          return false;
        }
      inStream.read(test, 3, 19);
      for(int i = 0; i < 22; i++)
        System.out.print((char)test[i]);

      foo = String.copyValueOf(test);
      if(handshakeIn.equals(foo)) {
        control.report(new String("Authorized connection."));
        outStream.write(message, 0, message.length());
        outStream.flush();
        control.newIncomingConnection(socketId);
        return true;
      }
      else {
        System.out.println("Could not handshake.");
        return false;
      }
    }
    catch(Exception e) {
      control.reportErr(new String("Server error: " + e));
      return false;
    }
  }

  /**
     Sends a byte stream
     @param buffer The byte array
     @param offset the distance from the beginning to send
     @return <code>true</code> if successful
  */
  public boolean send(byte[] buffer, int offset) {
    try {
      oS = new BufferedOutputStream(incoming.getOutputStream());
      oS.write(buffer, 0, offset);
      oS.flush();
      sent++;
      return true;
    }
    catch(Exception e) {
      control.reportErr(new String("SocketHandler " 
                                    + socketId + ": Error sending message\n "+ e));
      autodisconnect();
      return false;
      }
  }
  
  // fields
  private ControlHandler control;      // the control handler 
  private DownloadHandler downloadHandler;
                                       // the download handler 
  private int sent = 0;                // number of sent messages
  private int rec = 0;                 // number of received messages
  private static BufferedOutputStream oS;  // the output stream 
  private static BufferedInputStream iS;   // the input stream
  public  boolean live = true;         // true if the connection is live 
  private InetAddress inet;            // keeps track of information about
                                       // the remote host 
  private Socket incoming;             // the Socket to the remote host
  private int socketId;                // the unique ID of this SocketHandler 
  private static int SLEEP_LENGTH = 10; // default sleep length
}

