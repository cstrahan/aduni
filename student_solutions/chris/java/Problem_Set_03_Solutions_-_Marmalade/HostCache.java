/*
  Host cache - list of IP addresses to load on start up
*/
import java.util.Arrays;

public class HostCache
{
  public static Host[] hosts;
  
  /*  public static void loadCache()
  {
    hosts = Preferences.HOSTS_CACHE;
  }
  */    
  public static synchronized int getCount()
  {
    if (isNull()) return 0;
    else return (hosts.length);
  }
  
  public static String getIP(int i)
  {
    return (hosts[i].getName());
  }
  
  public static int getPort(int i)
  {
    return (hosts[i].getPort());
  }

  public static boolean isNull()
  {
    if (hosts == null) return true;
    else return false;
  }

  public static void addConnection(Connection c)
  {
    Host h = new Host(c.getIPAddress().toString(), c.getIPAddress().getPort());
    addHost(h);
  }
  
  public static synchronized void addHost(Host h)
  {
    if (isNull())
	  {
      hosts = new Host[1];
      hosts[0] = h;
      System.out.println("Host added in HostCache " + h.getName() + " total hosts " + getCount());
      Searcher.updateHostCache(h, true);
 	  }
    else if (!isPresent(h))
	  {
      Host[] temp = new Host[hosts.length + 1];
      System.arraycopy(hosts, 0, temp, 0, hosts.length);
      temp[hosts.length] = h;
      hosts = temp;
      System.out.println("Host added in HostCache " + h.getName() + " total hosts " + getCount());
      Searcher.updateHostCache(h, true);
 	  }
  }

  public static void removeConnection(Connection c)
  {
    Host h = new Host(c.getIPAddress().toString(), c.getIPAddress().getPort());
    removeHost(h);
  }

  public static synchronized void removeHost(Host h)
  {
    if (!(isNull()) && isPresent(h))
    {
      Host[] temp = new Host[hosts.length - 1];
      int j = 0;
      for (int i = 0; i < hosts.length; i++)
      {
        if (h.equals(hosts[i])) continue;
        temp[j] = hosts[i];
        j++;
      }
      hosts = temp;
      System.out.println("Host removed in HostCache " + h.getName() + " Hosts left " + getCount());
      Searcher.updateHostCache(h, false);
    }
  }

  public static synchronized boolean isPresent(Host h)
  {
    for (int i = 0; i < hosts.length; i++)
    {
      if (h.equals(hosts[i])) return true;
    }
    return false;
  }

  public static void connectHost(int i)
  {
    if (!isNull())
    {
      //System.out.println("Hosts in cache = " + getCount());
      System.out.println("Attempting to connect to " + getIP(i) + ":" + getPort(i));
      Connector connector = new Connector(getIP(i), getPort(i), 20000);
      connector.start();
    }
  }
}





    
      
