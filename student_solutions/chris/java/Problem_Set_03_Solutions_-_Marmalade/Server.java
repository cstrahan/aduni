import java.io.*;
import java.net.*;

class Server extends Thread
{
    BufferedInputStream in;
    IPAddress mine;
    
    public Server(Connection c)
    {
      in = c.getByteReader();
      mine = c.getIPAddress();
    }
    
  public void run()
  {
    while (true)
    {
      try
      {
        if (in.available() < Packet.HEADER_LENGTH)
			    continue;
        byte[] temp = new byte[Packet.HEADER_LENGTH];

        for (int i = 0; i < Packet.HEADER_LENGTH; i++)
			    temp[i] = (byte)in.read();
        Packet header = new Packet(temp);

        if ((header.identify() != Packet.PONG) && (header.identify() != Packet.PING) &&
            (header.identify() != Packet.QUERY) && (header.identify() != Packet.QUERYHIT))
			    break; // If the data is not something we expect, die.

        byte[] newpacket = new byte[(header.length() + Packet.HEADER_LENGTH)]; /* The syntax here is unfortunate, because headers don't store
												  their own size. */
        header.decrementTtl();
        header.incrementHops();

        for (int i = 0; i < Packet.HEADER_LENGTH; i++) // First fill the packet with the header.
			    newpacket[i] = temp[i];
        for (int i = Packet.HEADER_LENGTH; i < (header.length() + Packet.HEADER_LENGTH); i++) // Then fill the rest.
			    newpacket[i] = (byte)in.read();

        if (header.getTtl() < 0) // Kill old packets (but only after we've removed them from the input stream).
			    continue;

        if (header.identify() == Packet.PING) // We don't have to do any packet construction if all we've got is a Ping.
        {
          Ping ping = new Ping(newpacket);
          PingHandler handler = new PingHandler(mine, ping);
          handler.start();
          continue;
        }

        if (header.identify() == Packet.PONG)
        {
          Pong pong = new Pong(newpacket);
          Host h = new Host(mine.toString(), mine.getPort());
          HostCache.addHost(h);
          PongHandler handler = new PongHandler(mine, pong);
          handler.start();
          Pinger.inform(pong);
          continue;
        }
        else if (header.identify() == Packet.QUERY)
        {
          Query query = new Query(newpacket);
          QHandler handler = new QHandler(mine, query);
          handler.start();
          continue;
        }
        else
        {
          QueryHit queryhit = new QueryHit(newpacket);
          QHitHandler handler = new QHitHandler(mine, queryhit);
          handler.start();
          Searcher.inform(mine, queryhit);
        }
      }
      catch (Exception e)  // If there's a problem, we just die.
      {
        break;
      }
    }
    NetworkManager.notify(mine);
  }
}

