import java.util.Random;
import java.io.IOException;

public class Dice
{
    public static void main(String[] args)
    {
	System.out.println("You have six throws of a pair of dice.\n" +
			   "The objective is to get a double six. Here goes...\n");

	Random diceValues = new Random(); // Random number generator
	String[] theThrow = {"First ", "Second ", "Third ",
			     "Fourth ", "Fifth ", "Sixth "};

	int die1 = 0;
	int die2 = 0;

	for(int i = 0; i < 6 ; i++)
	    {
		die1 = 1 + Math.abs(diceValues.nextInt())%6; // Number from 1 to 6
		die2 = 1 + Math.abs(diceValues.nextInt())%6;
		System.out.println(theThrow[i] + "throw: " + die1 + ", " + die2);

		if(die1 + die2 == 12)
		    {
			System.out.println("You win!");
			return;
		    }
	    }
	System.out.println("Sorry, you lost...");
	return;
    }
}
