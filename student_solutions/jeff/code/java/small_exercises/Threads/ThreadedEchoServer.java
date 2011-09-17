import java.io.*;
import java.net.*;

public class ThreadedEchoServer
{
  public static void main(String[] args)
  {
    int i = 1;
    try
    {
      ServerSocket s = new ServerSocket(8189);
      for(;;)
      {
        Socket incoming = s.accept();
        System.out.println("Spawning " + i);
        new ThreadedEchoHandler(incoming, i).start();
        i++;
      }
    }
    catch(Exception e)
    {
      System.out.println(e);
    }
  }
}

class ThreadedEchoHandler extends Thread
{
  public ThreadedEchoHandler(Socket i, int c)
  {
    incoming = i;
    counter = c;
  }
  
  public void run()
  {
    try
    {
      BufferedReader in = new BufferedReader
        (new InputStreamReader(incoming.getInputStream()));
      PrintWriter out = new PrintWriter
        (incoming.getOutputStream(), true /* autoFlush */);

      out.println("Hello! Enter BYE to exit.");

      boolean done = false;
      while(!done)
      {
        String string = in.readLine();
        if (string == null) done = true;
        else
        {
          out.println("Echo (" + counter + "): " + string);
          if (string.trim().equals("BYE"))
            done = true;
        }
      }
      incoming.close();
    }
    catch(Exception e)
    {
      System.out.println(e);
    }
  }
  private Socket incoming;
  private int counter;
}


            
            
            
          
    
  
      
        
        
