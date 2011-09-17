import java.io.*;

public class TryPrimesOutput
{
    public static void main(String[] args)
    {
	// variables
	long[] primes = new long[200];             // Array to store primes
	primes[0] = 2;
	primes[1] = 3;
	int count = 2;      // number of primes found so far

	long number = 5;    // next integer to be tested

    outer:
	for( ; count < primes.length; number += 2L)
	    {
		// The maximum divisor we need to try is square root of number
		long limit = (long)Math.ceil(Math.sqrt((double)number));

		// Divide by all the primes we have up to limit
		for (int i = 1; i < count && primes[i] <= limit; i++)
		    if(number % primes[i] == 0)
			continue outer;

		primes[count++] = number;
	    }

	// Write the primes to a file
	try
	    {
		String dirName  = "/opt/home/jeff/code/java/FileWork";
		String fileName = "primes.bin";

		File myPrimeDir = new File(dirName);
		if(!myPrimeDir.exists())
		    myPrimeDir.mkdir();
		else
		    if(!myPrimeDir.isDirectory())
			{
			    System.err.println(dirName + " is not a directory");
			    return;
			}
		//Create the file object
		File primesFile = new File(myPrimeDir, fileName);
		primesFile.createNewFile();

		// Create a buffered data output stream for the dile
		DataOutputStream primesStream = new DataOutputStream
		    (new BufferedOutputStream
			(new FileOutputStream(primesFile)));

		// Write the primes to the file
		for(int i = 0; i < primes.length; i++)
		    primesStream.writeLong(primes[i]);

		primesStream.close();
		System.out.println("File size = " + primesStream.size());
	    }
	
	catch(IOException e)
	    {
		System.out.println("IOException " + e + " occurred");
	    }
    }
}
		

       
	
