/**
   MessageServer.java
   part of footella

   @author JMR
   @version $Id: MessageServer.java,v 1.16 2001/01/28 15:58:49 jeff Exp $

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
    connection = new SocketHandler[MAXCONNECTIONS];
    control = new ControlHandler(this);
    this.port = port;
    try {
      s = new ServerSocket(port); // Initialize the server

      /* Sets the IP Address. Not pretty at all. */
      Socket temp = new Socket("java.sun.com", 80); // bwahaha!
      Utility.setIpAddress(temp.getLocalAddress());
      Utility.setPort(port);
      System.out.println("Local address set at " + Utility.getAddress());
      temp.close();
    }
    catch(IOException e) { System.out.println("Cannot create ServerSocket."); }
    System.out.println("Server initialized and listening on port " + port);   
  }
  
  /**
     The main loop.
  */
  public void run() {
    try {
      for(;;) {
        if(socketId <= MAXCONNECTIONS) {
          Socket incoming = s.accept();
          System.out.println("Received connection request, spawning thread.");
          connection[socketId] = 
            new SocketHandler(incoming, socketId, incoming.getInetAddress(), control);
          if(connection[socketId].handshake(incoming)) {
            control.report(new String("Connected to " + incoming.getInetAddress()));
            connection[socketId].start();
            control.addConnection(connection[socketId], 0);
            socketId++;
          }
        }
        else
          System.out.println("Too many connections, cannot accept new connection.");
      }
    }
    catch(Exception e) {
      System.out.println("Server error in run(): " + e);
    }
  }


  /* A few accessors */
  public ControlHandler getControlHandler() {
    return control;
  }

  public static String getAddress(int socketId) {
    return connection[socketId].getAddress();
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
        if(socketId <= MAXCONNECTIONS) {
          System.out.println("Received connection request, spawning thread " + socketId);
          connection[socketId] = 
            new SocketHandler(outgoing, socketId, outgoing.getInetAddress(), control);
          connection[socketId].start();
          control.report(new String(("Opened connection to " + outgoing.getInetAddress() +
                             " on socket " + socketId)));
          control.addConnection(connection[socketId], 1);
          socketId++;
          return true;
        }
      }
      return false;
    }
    catch(ConnectException e) {
      System.out.println("Cannot connect to server");
      return false;
    }
    catch(IOException e) {
      System.out.println("An Error occured: " + e);
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

  /**
     Requests a socket to send a buffer. Should be going away soon...
  */
  public static boolean dispatch(byte[] buffer, int offset, int payload, int socketId) {
    connection[socketId].send(buffer, offset);
    return true;
  }

  /**
     Fetches a certain number of bytes from the incoming stream of a socket
     @param buffer the byte to write to
     @param length how many bytes to read
     @param socketId the socket to do the reading
     @return true on success
  */
  public static boolean getBytes(byte[] buffer, int length, int socketId) {
    connection[socketId].getBytes(buffer, length);
    return true;
  }

  // fields
  public static int port;
  public static int socketId = 0; 
  public static int MAXCONNECTIONS = 1024; // some number
  public static SocketHandler connection[];
  private static ServerSocket s;
  public static InetAddress ipAddress;
  public ControlHandler control;
}   
