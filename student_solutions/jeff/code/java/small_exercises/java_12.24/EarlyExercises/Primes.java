public class Primes
{
    public static void main(String[] args)
    {
	int nValues = 50;
	boolean isPrime = true;

	// Check all values from 2 to nValues
	for(int i = 2; i <= nValues; i++)
	    {
		isPrime = true;

		// Try dividing all integers from 2 to i-1
		for (int j = 2; j < i; j++)
		    {
			if(i % j == 0)
			    {
				isPrime = false;
				break;    // exit loop
			    }
		    }
		// Now we know if the number is prime
		if(isPrime)               // bool check
		    System.out.println(i);
	    }
    }
}
