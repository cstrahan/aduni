import java.net.*;
import java.io.*;
import java.util.Arrays;

public class Connector extends Thread
{
  public String GREETING = "GNUTELLA CONNECT/0.4";
  public String READY = "GNUTELLA OK";
  public byte[] greeting = (GREETING + "\n\n").getBytes();
  public byte[] ready = (READY + "\n\n").getBytes();
    
  private Connection connection;
  
  private static int TIMEOUT = 10000;

  // Constructor for making a connection to a servent
  public Connector (String aHost, int aPort, int t)
  {
    try
    {
      //Socket socket = SocketMaker.makeSocket(aHost, aPort, t);
      Socket socket = new Socket(aHost, aPort);
      connection = new Connection(socket, Connection.OUTGOING);
    }
    catch (IOException e)
    {
      //System.err.println(e);
    }
  }

  public Connector (String aHost, int aPort)
  {
    this(aHost, aPort, TIMEOUT);
  }

  public void run()
  {
    try
    {
      System.out.println("Connecting to " + connection.getIPAddress());
	
      connection.getByteWriter().write(greeting, 0, greeting.length);
      connection.getByteWriter().flush();
	
      String incoming = connection.getTextReader().readLine();
      String newline = connection.getTextReader().readLine();
        
      if (incoming == null || incoming.indexOf(READY) == -1)
	    {
        return;
	    }
      else
	    {
        HostArray.addConnection(connection);
        Host h = new Host(connection.getIPAddress().toString(), connection.getIPAddress().getPort());
        HostCache.addHost(h);    
        Server server = new Server(connection);
        server.start();
	    }
    }
    catch (Exception e)
    {
	    System.out.println("Connection failed.");
    }
  }
}

  




    
