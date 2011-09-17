public class Factorial
{
    public static void main(String[] args)
    {
	long limit = 5;
	long factorial = 1;

	// Loop from 1 to the value of limit
	for(int i = 1; i <= limit; i++)
	    {
		factorial = 1;
		for(int factor = 2; factor <= i; factor++)
		    factorial *= factor;
		System.out.println(i + "!" + " is " + factorial);
	    }
    }
}
