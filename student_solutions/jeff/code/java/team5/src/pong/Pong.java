public class Pong extends MessageObject
{
    private int port;
    private byte[] ipAddress = new byte[4];
    private int sharedFiles;
    private int kilobytes;


    /**
       Constructor
    */
    public Pong(byte[] messageId, int payloadDescriptor,
		int ttl, int hops, int payloadLength,
		int port, byte[] ipAddress, int sharedFiles, int kilobytes)
    {
	super(messageId, payloadDescriptor, ttl, hops, payloadLength);
	this.port = port;
	System.arraycopy(ipAddress, 0, this.ipAddress, 0, 4);
	this.sharedFiles = sharedFiles;
	this.kilobytes = kilobytes;
    }	
    
    public int getPort()
    {
	return port;
    }

    public byte[] getIpAddress()
    {
	byte[] temp = new byte[4];
	System.arraycopy(ipAddress, 0, temp, 0, 4);
	return temp;
    }

    public int getSharedFiles()
    {
	return sharedFiles;
    }

    public int getKilobytes()
    {
	return kilobytes;
    }
    
}






