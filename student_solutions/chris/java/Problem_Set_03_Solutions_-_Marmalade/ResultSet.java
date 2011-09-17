public class ResultSet
{
    private int count = 0;
    private int size; // The number of entries in the result set.
    private int numbytes = 0; // So that the QueryHit constructor can know how much data it will get in total.
    private int[] indices;
    private int[] filesizes;
    private String[] filenames;

    public ResultSet(int numfiles) // Create a ResultSet of a given size
    {
	size = numfiles;
	indices = new int[numfiles];
	filesizes = new int[numfiles];
	filenames = new String[numfiles];
    }

    public void addResult(int index, int filesize, String name)
    {
	indices[count] = index;
	filesizes[count] = filesize;

	byte[] test = name.getBytes(); // Test to make sure there aren't any weird characters that could screw up our system.
	for (int i = 0; i < test.length; i++)
	    if ((test[i] < 32) || (test[i] > 127))
		{
		    name = "---Invalid Filename---";
		    break;
		}
	filenames[count] = name;
	numbytes = (numbytes + 8 + name.length()); // 4 bytes for the file index, 4 bytes for the size, and as many bytes as necessary for the name.
	count++;

	if (count == size) // If we've finished adding results, we'll set count up for retrieval.
	    count = 0;
    }

    public int getNumBytes()
    {
	return (numbytes);
    }

    public int getSize()
    {
	return (size);
    }

    public boolean more() /* More returns true as long as the QueryHit constructor still has information to parse, provided it calls the following operations in
			     the right order. */
    {
	return (count < size);
    }

    /* This is a bit complicated, but it's the best idea I have at the moment.  Once the ResultSet is full, count will be set back to zero.  Our QueryHit
       constructor will call the next three methods *in order* over and over again so that it can build the proper byte arrays. */

    public int getIndex()
    {
	return (indices[count]);
    }

    public int getFilesize()
    {
	return (filesizes[count]);
    }

    public String getName()
    {
	count++; // Increment the counter for the next trip through the selectors.
	return (filenames[(count - 1)]);
    }
}
