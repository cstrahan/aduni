public class FindPrimes
{
    public static void main(String[] args)
    {
	int nPrimes = 10;
	
    OuterLoop:
	for(int i = 2; ;i++)  // inifite loop
	    {
		// Try dividing by all integers from 2 to i-1
		for(int j = 2; j < i; j++)
		    {
			if(i % j == 0)
			    continue OuterLoop;
		    }
		// We get here when we have a prime
		System.out.println(i);
		if(--nPrimes == 0)
		    break;
	    }
    }
}
