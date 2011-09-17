import java.io.*;

public class PrimeCharacters
{
    public static void main(String[] args)
    {
	long[] primes = new long[20];
	primes[0] = 2;
	primes[1] = 3;
	int count = 2;

	long number = 5;

    outer:
	for( ; count < primes.length; number += 2)
	    {
		long limit = (long)Math.ceil(Math.sqrt((double)number));

		for(int i = 1; i < count && primes[i] <= limit; i++)
		    if(number%primes[i] == 0)
			continue outer;

		primes[count++] = number;
	    }

	// Output the primes array using a buffered stream
	PrintWriter output = new PrintWriter
	    (new BufferedWriter(new FileWriter(FileDescriptor.out)));

	for(int i = 0; i < primes.length; i++)
	    // New line after every fifth prime
	    output.print((i%5 == 0 ? "\n" : "    ") + primes[i]);
	output.print("\n\n");
	output.close();
    }
}


		
	 
