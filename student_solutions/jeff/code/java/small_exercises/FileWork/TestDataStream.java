import java.io.*;
import java.util.Date;

public class TestDataStream
{
    public static void main(String[] args)
    {
	String myStr = new String("Garbage in, garbage out");
	String dirName = "/home/jeffrey/code/java/FileWork"; // Directory name

	try
	    {
		File dir = new File(dirName); // File object for dir
		if(!dir.exists())
		    dir.mkdir();
		else
		    if(!dir.isDirectory())
		    {
			System.err.println(dirName + " is not a directory");
			return;
		    }

		File aFile = new File(dir, "data.txt");
		aFile.createNewFile();            // Now create a file if nec.

		// Create the byte output stream
		DataOutputStream myStream = new DataOutputStream
		    (new FileOutputStream(aFile));
		myStream.writeChars(myStr); // Write the string to a file
	    }
	catch(IOException e)
	    {
		System.out.println("IO exception thrown: " + e);
	    }
    }
}
		
								 
		    
		
		    
			
