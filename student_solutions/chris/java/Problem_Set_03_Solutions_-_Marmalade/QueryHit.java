public class QueryHit extends Packet // QueryHits are a little more complicated, because we'll need to generate a result set and use a ServentID.
{
    public QueryHit(int numHits, int port, IPAddress ip, int speed, ResultSet result, byte[] id, byte[] messageid)
    {
	super(Packet.QUERYHIT, (27 + (result.getNumBytes() + (2 * result.getSize())))); // We're supposed to stick in two 0 bytes after each filename string.

	for (int i = 0; i < messageid.length; i++) // QueryHits need to have the same message ID as the generating Query, so we need to pass it to the constructor.
	    contents[i] = messageid[i];

	contents[23] = (byte)numHits;
	contents[25] = (byte)(port >>> 8);
	contents[24] = (byte)(port & 0xff);
	contents[26] = (byte)ip.getFirst();
	contents[27] = (byte)ip.getSecond();
	contents[28] = (byte)ip.getThird();
	contents[29] = (byte)ip.getFourth();
	contents[33] = (byte)(speed >>> 24);
	contents[32] = (byte)((speed & 0xffffff) >>> 16);
	contents[31] = (byte)((speed & 0xffff) >>> 8);
	contents[30] = (byte)(speed & 0xff);
	
	int i = 34;
	while (result.more())
	    {
		int index = result.getIndex();  // Store the file index as four separate bytes.
		contents[i] = (byte)(index & 0xff);
		i++;
		contents[i] = (byte)((index & 0xffff) >>> 8);
		i++;
		contents[i] = (byte)((index & 0xffffff) >>> 16);
		i++;
		contents[i] = (byte)(index >>> 24);
		i++;

		int size = result.getFilesize(); // Store the file size as four seperate bytes.
		contents[i] = (byte)(size & 0xff);
		i++;
		contents[i] = (byte)((size & 0xffff) >>> 8);
		i++;
		contents[i] = (byte)((size & 0xffffff) >>> 16);
		i++;
		contents[i] = (byte)(size >>> 24);
		i++;

		String name = result.getName(); // Convert the name string into a byte array and store it at the right place in the overall contents.
		byte[] temp = new byte[name.length()];
		temp = name.getBytes();

		for (int j = 0; j < name.length(); j++)
		    {
			contents[i + j] = temp[j];
		    }

		i = i + name.length();
		contents[i] = 0;  // Result fields are double-zero delimited.
		i++;
		contents[i] = 0;
		i++;
	    }
	
	// Once we've figured out ServentID, we have to put it at the end of our byte array here.
	// For now, we fill it with zeroes.

	for (int j = 0; j < 16; j++)
	    {
		contents[(i + j)] = id[j];
	    }
    }

    public QueryHit(byte[] rawdata)
    {
	super(rawdata);
    }

    public int getNumHits()
    {
	return (contents[23]);
    }

    public int getPort()
    {
	return (((contents[25] & 0xff)<< 8) | (contents[24] & 0xff));
    }

    public IPAddress getIP()
    {
	return (new IPAddress((contents[26] & 0xff), (contents[27] & 0xff), (contents[28] & 0xff), (contents[29] & 0xff), (getPort())));
    }

    public int getSpeed()
    {
	return (((contents[33] & 0xff) << 24) | ((contents[32] & 0xff) << 16) | ((contents[31] & 0xff) << 8) | (contents[30] & 0xff));
    }

    public ResultSet getResults()
    {
	ResultSet results = new ResultSet((int)contents[23]); // This byte holds the number of results in the set.

	int i = 34;
	int count = 0;

	while ((count < contents[23]) && (i < (totalLength() - 26))) // The last test is to catch poorly-made query hits.
	    {
		int index = (((contents[(i + 3)] & 0xff) << 24) | ((contents[(i + 2)] & 0xff) << 16) | ((contents[(i + 1)] & 0xff) << 8) | (contents[i] & 0xff));
		i = i + 4;
		int size = (((contents[(i + 3)] & 0xff) << 24) | ((contents[(i + 2)] & 0xff) << 16) | ((contents[(i + 1)] & 0xff) << 8) | (contents[i] & 0xff));
		i = i + 4;

		String name = "";
		while (!((contents[i] == 0) && (contents[(i + 1)] == 0)))
		    {
			name = name + (char)contents[i];
			i++;
		    }

		results.addResult(index, size, name);

		i = i + 2; // Account for the two zero bits.
		count++;
	    }

	return (results);
    }
}

