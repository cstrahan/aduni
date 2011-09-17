public class NumberCheck
{
    public static void main(String[] args)
    
    {
	int number = 0;
	number = 1+(int) (100*Math.random()); // Random number between 1 and 100

	if(number%2 == 0)   // Test for evenness
	    {
		if(number <= 50)
		    System.out.println("You have got an even number <= 50, "
				       + number);
		else
		    System.out.println("You have got an even number > 50, "
				       + number);
	    }
	//System.out.println("You have got an even number, " + number); // true
	else
	    System.out.println("You have got an odd number, " + number); // false
    }
}
