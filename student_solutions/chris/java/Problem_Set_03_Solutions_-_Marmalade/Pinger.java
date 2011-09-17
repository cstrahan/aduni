public class Pinger extends Thread
{
    static int hosts = 0;
    static int totalkb = 0;
    static int totalfiles = 0;
    static Ping myping;

    public void run()
    {
	while(true)
	    {
		try
		    {
			sleep(Preferences.PINGER_TIME);
		    }
		catch (Exception e)
		    {
			System.out.println(e.getMessage());
		    }
		Searcher.updateInfo(hosts, totalkb, totalfiles);
		myping = new Ping();
		hosts = 0;
		totalkb = 0;
		totalfiles = 0;
		NetworkManager.writeToAll(myping);
	    }
    }

    public static void inform(Pong pong)
    {
	if (pong.compare(myping))
	    {
		hosts++;
		totalfiles += pong.getNumFiles();
		totalkb += pong.getKb();
	    }
    }
	
}
