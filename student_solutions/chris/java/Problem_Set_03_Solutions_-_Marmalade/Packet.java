class Packet
{
    public static final int HEADER_LENGTH = 23;
    public static final byte PING = 0;
    public static final byte PONG = 1;
    public static final byte QUERY = -128; // Twos complement for 128 -- could also try (byte)128
    public static final byte QUERYHIT = -127; // Likewise for 129 -- could also try (byte)129
    public static final byte TTL = 7; // Standard Time to Live for a new packet
    public static final byte HOPS = 0; // All new packets start at zero hops

    protected byte[] contents;

    public Packet(byte payload, int length)
    {
	contents = new byte[length + HEADER_LENGTH]; // Length does _not_ include the length of the descriptor, so we have to put it in here.

	for (int i = 0; i < 16; i++)
	    {
		contents[i] = (byte)((255 * Math.random()) - 128); // Unique Message ID, and more twos complement problems
	    }
	contents[16] = payload; //Payload descriptor
	contents[17] = TTL; // Time to Live
	contents[18] = HOPS; // Hops so far
	contents[22] = (byte)(length >>> 24); // Extract the biggest byte of the integer
	contents[21] = (byte)((length & 0xffffff) >>> 16); // Extract the 2nd byte
	contents[20] = (byte)((length & 0xffff) >>> 8); // Ditto the third
	contents[19] = (byte)(length & 0xff); // Last byte
    }

    public Packet(byte[] rawdata)
    {
	contents = rawdata;
    }

    public byte identify() // Identifies the type of packet.  Compare to Packet.PING, Packet.QUERYHIT, etc.
    {
	return (contents[16]);
    }

    public int length() // Upshift each byte while masking out the awful Java sign extension, then bitwise OR them all together.
    {
	int length = (((contents[22] & 0xff) << 24) | ((contents[21] & 0xff) << 16) | ((contents[20] & 0xff) << 8) | (contents[19] & 0xff));
	return (length);
    }

  public int totalLength()
  {
    return (this.length() + HEADER_LENGTH);
  }
  

    public int getTtl()
    {
	return ((int)contents[17]);
    }

    public int getHops()
    {
	return ((int)contents[18]);
    }

    public void decrementTtl()
    {
	(contents[17])--;
    }

    public void incrementHops()
    {
	(contents[18])++;
    }
    
    public byte[] contents()
    {
	return (contents);
    }

    public boolean compare(Packet tocompare)
    {
	for (int i = 0; i < 16; i++)
	    {
		if (contents[i] != tocompare.contents[i])
		    return (false);
	    }
	return(true);
    }

    //method to obtain messageID from packet
    public byte[] getMessageID()
    {
	byte[] messageID = new byte[16];
	for (int i = 0; i < 16; i++)
	    {
		messageID[i] = contents[i];
	    }
	return (messageID);
    }
    //	messageID = System.arraycopy(contents, 0, messageID, 0, 16);

    public int hashcode()
    {
	int hashcode = 0;
	for (int i = 0; i < 16; i++)
	    {
		hashcode += (int) contents[i];
	    }
	return (hashcode);
    }

    public boolean equals (Packet p)
    {
	for (int i = 0; i < 16; i++)
	    {
		if (contents[i] != p.contents[i])
		    return (false);
	    }
	return(true);
    }	
}

