public class Push extends MessageObject
{
    private byte[] serventID = new byte[16];
    private int fileIndex;
    private byte[] ipAddress = new byte[4];
    private int port;

    /**
       Constructor
    */
    public Push(byte[] messageId, int payloadDescriptor,
		int ttl, int hops, int payloadLength,
		byte[] serventID, int fileIndex, byte[] ipAddress, int port)
    {
	super(messageId, payloadDescriptor, ttl, hops, payloadLength);
	System.arraycopy(serventID, 0, this.serventID, 0, 16);
	this.fileIndex = fileIndex;
	System.arraycopy(ipAddress, 0, this.ipAddress, 0, 4);
	this.port = port;
    }

    public byte[] getServentID()
    {
	byte[] temp = new byte[16];
	System.arraycopy(serventID, 0, temp, 0, 16);
	return temp;
    }
    
    public int getFileIndex() { return fileIndex; }
    public byte[] getIpAddress()
    {
	byte[] temp = new byte[4];
	System.arraycopy(ipAddress, 0, temp, 0, 4);
	return temp;
    }
    public int getPort() { return port; }

}








