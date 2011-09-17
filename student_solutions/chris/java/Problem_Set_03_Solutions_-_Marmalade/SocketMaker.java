/*
  Socket Maker - timeout int milliseconds
*/
import java.io.*;
import java.net.*;

class SocketMaker implements Runnable
{
  private String host;
  private int port;
  private Socket socket;
  
  public SocketMaker(String aHost, int aPort)
  {
    socket = null;
    host = aHost;
    port = aPort;
  }

  public static Socket makeSocket(String aHost, int aPort, int timeout)
  {
    SocketMaker maker = new SocketMaker(aHost, aPort);
    Thread t = new Thread(maker);
    t.start();
    try
    {
      t.join(timeout);
    }
    catch (InterruptedException e)
    {
    }
    return maker.getSocket();
  }
   
  public void run()
  {
    try
    {
      socket = new Socket(host, port);
    }
    catch (UnknownHostException e)
    {
      System.err.println(e);
    }
    catch (IOException e)
    {
      System.err.println(e);
    }
  }
  
  public Socket getSocket()
  {
    return socket;
  }

  public static void main(String[] args)
  {
    int timeout = 5000;
    Socket s = SocketMaker.makeSocket("www.sun.com", 80, timeout);
    System.out.println("opened socket");
  }
}


        
        
