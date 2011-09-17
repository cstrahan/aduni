
/**
 * ReadZippedPrimes.java
 *
 *
 * Created: Wed Dec 27 15:43:58 2000
 *
 * @author 
 * @version
 */

public class ReadZippedPrimes /**
 * ReadZippedPrimes.java
 *
 *
 * Created: Wed Dec 27 14:34:57 2000
 *
 * @author 
 * @version
 */

import java.io.*;
import java.util.zip.*;

class ReadZippedPrimes
{
    public static void main(String[] args)
    {
	try
	    {
		// Create a default fomatted char output stream
		FormatWriter out = new FormatWriter
		    (new BufferedWriter
			(new FileWriter(FileDescriptor.out)));
		String dirName = "/opt/home/jeff/code/java/FileWork";
		String zipName = "NewPrimes.zip";

		File myPrimeZip = new File(dirName, zipName);
		ZipInputStream myZipFile = new ZipInputStream
		    (new FileInputStream(myPrimeZip));
		ZipEntry myZipEntry = myZipFile.getNextEntry();

		out.println("Compressed File is " + myZipEntry.getName());
		DataInputStream primesIn = new DataInputStream
		    (new BufferedInputStream(myZipFile));
		
		long[] primes = new long[6];
		boolean EOF = false;
		
		while(!EOF)
		    {
			int index = 0;
			try
			    {
				// Fill the array with primes from the file
				for (index = 0; index < primes.length; index++)
				    primes[index] = primesIn.readLong();
			    }
			catch (EOFException e)
			    {
				EOF = true;
			    }
			// Output the number of primes int the array
			for(int j = 0; j < index; j++)
			    out.print(primes[j]);
			out.println();
		    }
		out.close();
		primesIn.close();
	    }
	catch(FileNotFoundException e)
	    {
		System.err.println(e);
		return;
	    }
	catch(IOException e)
	    {
		System.out.println("Error reading input file" + e);
		return;
	    }
    }
}
























   



















































