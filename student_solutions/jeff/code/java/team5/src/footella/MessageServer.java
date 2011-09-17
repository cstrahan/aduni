/**
   MessageServer.java
   part of footella

   @author JMR
   @version $Id: MessageServer.java,v 1.20 2001/01/29 22:48:23 jeff Exp $

   This class is responsible for handling new incoming and outgoing connections.
*/

import java.io.*;
import java.net.*;

public class MessageServer extends Thread {
  /**
     Constructor
     @param port The port number to listen on
  */
  MessageServer(int port) { 
    System.out.println("\n               < Footella 1.0 >\n");

    // Initialize the the array of SocketHandlers and
    // the control handler
    control = new ControlHandler(this);
    this.port = port;


    try {
      // Initialize the server
      s = new ServerSocket(port);

      /* Sets the IP Address. Not pretty at all. */
      Socket temp = new Socket("java.sun.com", 80); // bwahaha!
      Utility.setIpAddress(temp.getLocalAddress());
      Utility.setPort(port);
      System.out.println("Local address set at " + Utility.getAddress());
      temp.close();
    }
    catch(IOException e) { System.out.println("Cannot create ServerSocket."); }
    // success
    System.out.println("Server initialized and listening on port " + port);   
  }
  
  /**
     The main loop.
  */
  public void run() {
    try {
      for(;;) {
        Socket incoming = s.accept();
        // System.out.println("Received connection request, spawning thread.");
        SocketHandler connection = 
          new SocketHandler(incoming, socketId, incoming.getInetAddress(), control);
        if(connection.handshake(incoming)) {
          control.report(new String("Connected to " + incoming.getInetAddress()));
          connection.start();
          control.addConnection(connection, 0);
          Utility.currentIncoming++;
          control.updateNumberOfConnections();
          socketId++;
        }
      }
    }
    catch(SocketException e) {
      System.out.println("Socket closed");
    }
    catch(Exception e) {
      System.out.println("Server error in run(): " + e);
    }
  }


  /* A few accessors */
  public ControlHandler getControlHandler() {
    return control;
  }

  /**
     Opens a new outgoing connection.
     @param domain
     @param port
     @return <code>true</code> on success
  */
  public boolean openConnection(String domain, int port) {
    try {
      Socket outgoing = new Socket(domain, port);
      
      if(handshake(outgoing)) {
        if(Utility.currentOutgoing <= Utility.MAX_OUTGOING) {
          // System.out.println("Received connection request, spawning thread " + socketId);
          connection = 
            new SocketHandler(outgoing, socketId, outgoing.getInetAddress(), control);
          connection.start();
          control.report(new String(("Opened connection to " + outgoing.getInetAddress() +
                             " on socket " + socketId)));
          control.addConnection(connection, 1);
          socketId++;
          Utility.currentOutgoing++;
          control.updateNumberOfConnections();
          return true;
        }
      }
      else control.report("Unable to open new connection. " +
                          "Maximum outgoing connection limit reached");
      return false;
    }
    catch(ConnectException e) {
      control.reportErr("Cannot connect to server");
      return false;
    }
    catch(IOException e) {
      control.reportErr(new String("An Error occured: " + e));
      return false;
    }
  }

  /**
     Controls the handshake with incoming connections.
     Will unmercifully dump the noncompliant.
     @param s The connection candidate
     @return <code>true</code> on success
  */
  private static boolean handshake(Socket s) {
    try {
      BufferedReader inStream = 
        new BufferedReader(new InputStreamReader(s.getInputStream()));
      Writer outStream = 
        new BufferedWriter(new OutputStreamWriter(s.getOutputStream()));
      
      String handshakeIn = "GNUTELLA CONNECT/0.4\n\n";
      String handshakeOut = "GNUTELLA OK\n\n";

      // Try to send a message
      System.out.println("Negotiating Handshake... ");
      outStream.write(handshakeIn, 0, handshakeIn.length());
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

  // fields
  public static int port;
  public static int socketId = 0; 
  private SocketHandler connection;
  private static ServerSocket s;
  public static InetAddress ipAddress;
  public ControlHandler control;
}   
