public class MorePrimes
{
    public static void main(String[] args)
    {
	long[] primes = new long[6];
	primes[0] = 2;
	primes[1] = 3;
	int count = 2;
	long number = 5;

    outer:
	for ( ; count < primes.length; number += 2)
	    {
		// The maximum divisor we need to try is square root of number
		long limit = (long)Math.ceil(Math.sqrt((double)number));
		// Divide by all the primes we have up to limi
		for(int i = 1; i < count && primes[i] <= limit; i++)
		    if(number%primes[i] == 0)
			continue outer;

		primes[count++] = number;
	    }
	for(int i=0; i < primes.length; i++)
	    System.out.println(primes[i]);
    }
}
