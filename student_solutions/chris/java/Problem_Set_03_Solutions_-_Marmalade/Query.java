public class Query extends Packet
{
    private IPAddress ip;

    public Query(int speed, String search)
    {
	super(Packet.QUERY, (3 + (search.length())));
	contents[24] = (byte)(speed >>> 8); // Convert minimum speed to 2 bytes
	contents[23] = (byte)(speed & 0xff);
	
	byte[] temp = new byte[search.length()];
	temp = search.getBytes();
	
	int i;
	for(i = 0; i < search.length(); i++)
	    {
		contents[(i + 25)] = temp[i];
	    }
	contents[(i + 25)] = 0; // Search strings should be 0-terminated.
	ip = null;  //initialize IPaddress to null.
    }
    
    public Query(byte[] rawdata)
    {
	super(rawdata);
    }
    
    public IPAddress getIP()
    {
	return (ip);
    }

    public int getSpeed()
    {
	return (((contents[24] & 0xff) << 8) | (contents[23] & 0xff));
    }

    public String getSearchString()
    {
	String answer = "";
	for (int i = 25; contents[i] != 0; i++)
	    {
		answer = answer + (char)(contents[i]);
	    }
	return (answer);
    }

    public void setIP(IPAddress ip)
    {
	this.ip = ip;
    }
}




