import java.io.*;

public class ReadPrimes
{ 
    public static void main(String[] args)
    {
	try
	    {
		// Create a File object and an input stream object for the file
		String directory = "/opt/home/jeff/code/java/FileWork";
		String fileName =  "Primes.bin";
		File myPrimes = new File(directory, fileName);

		DataInputStream primesIn = new DataInputStream
		    (new FileInputStream(myPrimes));

		// Code to read the primes from the stream - listed below
		// Create a default formatted character output stream
		FormatWriter out = new FormatWriter
		    (new BufferedWriter(new FileWriter(FileDescriptor.out)));

		long[] primes = new long[6];
		boolean EOF = false;

		while(!EOF)		    
		    {
			int index = 0;
			try
			    {
				// Fill the array with primes from the file
				for(index = 0; index < primes.length; index++)
				    primes[index] = primesIn.readLong();
			    }
			catch(EOFException e)
			    {
				EOF = true;
			    }

			// Output the number of primes in the array
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
		System.err.println("Error reading input file" + e);
		return;
	    }
    }
}


    
