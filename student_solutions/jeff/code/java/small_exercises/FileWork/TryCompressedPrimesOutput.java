import java.io.*;
import java.util.zip.*;

public class TryCompressedPrimesOutput
{
    public static void main(String[] args)
    {
	long[] primes = new long[200];   // Array to store primes
	primes[0] = 2;
	primes[1] = 3;
        int count = 2;   // count of primes found so far

	long number = 5;

    outer:
	for( ; count < primes.length; number += 2L)
	    {
		// Get maximum divisor
		long limit = (long)Math.ceil(Math.sqrt((double)number));

		// Divide by all the primes we have up to limit
		for(int i = 1; i < count && primes[i] <= limit; i++)
		    if(number%primes[i] == 0)
			continue outer;

		primes[count++] = number;
	    }

	// Write the primes to a file
	try
	    {
		String dirName = "/opt/home/jeff/code/java/FileWork";
		String zipName = "NewPrimes.zip";
		String fileName = "NewPrimes.bin";
		File myPrimeDir = new File(dirName);

		if(!myPrimeDir.exists())  // If it doesn't exist
		    myPrimeDir.mkdir();   // make it
		else
		    if(!myPrimeDir.isDirectory())
			{
			    System.err.println(dirName + " is not a directory");
			    return;
			}

		File myPrimeZip = new File(myPrimeDir, zipName);
		myPrimeZip.createNewFile();

		// Create the zip output stream
		ZipOutputStream myZipFile = new ZipOutputStream
		    (new FileOutputStream(myPrimeZip));
		
		// Create the zip entry for the file and write it to the zip output
		ZipEntry myZipEntry = new ZipEntry(fileName);
		myZipFile.putNextEntry(myZipEntry);

		// Create the output stream to the zip output stream
		DataOutputStream myFile = new DataOutputStream
		    (new BufferedOutputStream(myZipFile));

		// Write primes to the file
		for(int i = 0; i < primes.length; i++)
		    myFile.writeLong(primes[i]);

		myFile.flush();               // Make sure all is written
		myZipFile.closeEntry();         // End the ZIP entry
		myFile.close();               // Close the file system
		System.out.println("File size = " + myFile.size());
		System.out.println("Compressed file size = " + myZipEntry.getCompressedSize());
	    }
	catch(IOException e)
	    {
		System.out.println("Feh! IOException " + e +  " occurred.");
	    }
    }
}
		    
						      
