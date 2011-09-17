import java.util.*;

class PingHandler extends Thread
{
    public static Map pt;  //ping table    
    Ping ping;

    public PingHandler (IPAddress pingIP, Ping ping)
    {
	this.ping = ping;
	ping.setIP (pingIP);  //set ping's IP Address
    }

    public static void initPingTable()
    {
	pt = new Hashtable (5000);
    }

    public void run()
    {
	if (!pt.containsKey(ping))  //check that ping is not already in table
	    {
		NetworkManager.writeButOne(ping.getIP(), ping);
		pt.put ((Packet) ping, ping);
		Pong response = new Pong(Mine.getPort(), Mine.getIPAddress(), SharedDirectory.getOurNumFiles(),
					 SharedDirectory.getOurKb(), ping.getMessageID());
		NetworkManager.writeToOne(ping.getIP(), response);
	    }
    }
}


