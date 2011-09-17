/**
   Host has a string IP address and int port
*/
import java.net.*;

public class Host
{
  private String hostName;
  private int hostPort;
    
  public Host(String aHostName, int aHostPort)
  {
    hostName = aHostName;
    hostPort = aHostPort;
  }
  
  public String getName()
  {
    return hostName;
  }
  
  public int getPort()
  {
    return hostPort;
  }

  public boolean equals(Host h)
  {
    return ((hostName.equals(h.hostName)) && (hostPort == h.hostPort));
  }
}
