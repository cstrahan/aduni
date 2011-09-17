public class ForLoop
{
    public static void main(String[] args)
    {
	int limit = 20;
	int sum = 0;

	// Loop from 1 to the value of limit, adding 1 each cycle
	for(int i = 1; i <= limit; i++)
	    {
		sum += i;
		System.out.println("the sum is now = " + sum);
	    }
	System.out.println("Total sum is = " + sum);

    }
}
