import java.io.*;
import java.util.*;

public class Preferences
{
  public static String  FILE_NAME = "preferences.txt";
  public static int MAX_LIVE = 5;
  public static int MAX_CACHE = 100;
  public static boolean AUTO_CONNECT = true;
  public static int PINGER_TIME = 10000;
  public static int CONNECTOR_TIME = 10000;
  
  public static String SHAREPATH = "/opt";
  public static String SAVEPATH = "/home/ccrick";
      
  public static void readFromFile()
  {
    try
    {
          BufferedReader fileIn = new BufferedReader(new FileReader(FILE_NAME));
          String line;
          while ((line = fileIn.readLine()) != null)
          {
            if (line.startsWith("Host: "))
            {
              String address = line.substring(6);
              StringTokenizer t = new StringTokenizer(address, ":");
              Host h = new Host(t.nextToken(), Integer.parseInt(t.nextToken()));
              HostCache.addHost(h);
              continue;
            }
            else if (line.startsWith("Max-Live: "))
            {
              MAX_LIVE = Integer.parseInt(line.substring(10));
              continue;
            }
            else if (line.startsWith("Max-Cache: "))
            {
              MAX_CACHE = Integer.parseInt(line.substring(11));
              continue;
            }
            else if (line.startsWith("Auto-Connect: "))
            {
              AUTO_CONNECT = ((Boolean.valueOf(line.substring(14))).booleanValue());
              continue;
            }
            else if (line.startsWith("Pinger-Time: "))
            {
              PINGER_TIME = Integer.parseInt(line.substring(13));
              continue;
            }
            else if (line.startsWith("Connector-Time: "))
            {
              CONNECTOR_TIME = Integer.parseInt(line.substring(16));
              continue;
            }
            else if (line.startsWith("Shared-Directory: "))
            {
              SHAREPATH = line.substring(18);
              System.out.println("Shared-Directory is " + SHAREPATH);
              continue;
            }
            else if (line.startsWith("Download-Directory: "))
            {
              SAVEPATH = line.substring(20);
              System.out.println("Download-Directory is " + SAVEPATH);
              continue;
            }
          }
          fileIn.close();
    }
        
    catch (IOException e)
    {
      System.out.println("Unable to read preferences file");
    }
  }
  
  
  public static void writeToFile()
  {
    try
    {
          PrintWriter fileOut = new PrintWriter(new FileWriter(FILE_NAME));
          for (int i = 0; i < HostCache.getCount(); i++)
          {
            fileOut.println("Host: " + HostCache.hosts[i].getName() + ":" + HostCache.hosts[i].getPort());
          }
          fileOut.println("Max-Live: " + MAX_LIVE);
          fileOut.println("Max-Cache: " + MAX_CACHE);
          fileOut.println("Auto-Connect: " + AUTO_CONNECT);
          fileOut.println("Shared-Directory: " + SHAREPATH);
          fileOut.println("Download-Directory: " + SAVEPATH);

          System.out.println("Written to file");
          fileOut.close();
    }
    catch (IOException e)
    {
      System.out.println("Unable to write to preferences file");
    }
  }
}

    
      
