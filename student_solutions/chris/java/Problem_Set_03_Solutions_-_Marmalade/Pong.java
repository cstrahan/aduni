public class Pong extends Packet
{
  private int index = HEADER_LENGTH;

    public Pong(int port, IPAddress ip, int numberOfFiles, int kb, byte[] messageid)
    {
	super(Packet.PONG, 14);
  
	for (int i = 0; i < messageid.length; i++) // Pongs need the same Message IDs as the pings that generate them.
	    contents[i] = messageid[i];

	// convert port to two bytes
	contents[index + 0] = (byte)(port >>> 8);
	contents[index + 1] = (byte)(port & 0xff);
	
	// convert ip address to 4 bytes; need to check format of ip
	// address -- Little Endian????
	contents[index + 2] = (byte)ip.getFirst();
	contents[index + 3] = (byte)ip.getSecond();;
	contents[index + 4] = (byte)ip.getThird();
	contents[index + 5] = (byte)ip.getFourth();
	
	// convert number of files  to 4 bytes
	contents[index + 9] = (byte)(numberOfFiles >>> 24);
	contents[index + 8] = (byte)((numberOfFiles & 0xffffff) >>> 16);
	contents[index + 7] = (byte)((numberOfFiles & 0xffff) >>> 8);
	contents[index + 6] = (byte)(numberOfFiles & 0xff);
	
	// convert total kilobytes to 4 bytes
	contents[index + 13] = (byte)(kb >>> 24);
	contents[index + 12] = (byte)((kb & 0xffffff) >>> 16);
	contents[index + 11] = (byte)((kb & 0xffff) >>> 8);
	contents[index + 10] = (byte)(kb & 0xff);
    }

    public Pong(byte[] rawdata)
    {
	super(rawdata);
    }

    public int getPort()
    {
	int port = (((contents[index + 1] & 0xff) << 8) | (contents[index + 0] & 0xff));
	return (port);
    }

    public IPAddress getIP()
    {
	return (new IPAddress((contents[index + 2] & 0xff), (contents[index + 3] & 0xff), (contents[index + 4] & 0xff), (contents[index + 5] & 0xff), getPort()));
    }

    public int getNumFiles()
    {
	int numfiles = (((contents[index + 9] & 0xff) << 24) | ((contents[index + 8] & 0xff) << 16) | ((contents[index + 7] & 0xff) << 8) | (contents[index + 6] & 0xff));
	return (numfiles);
    }

    public int getKb()
    {
	int kb = (((contents[index + 13] & 0xff) << 24) | ((contents[index + 12] & 0xff) << 16) | ((contents[index + 11] & 0xff) << 8) | (contents[index + 10] & 0xff));
	return (kb);
    }
}



