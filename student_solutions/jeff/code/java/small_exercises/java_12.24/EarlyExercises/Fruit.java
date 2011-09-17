import java.io.IOException; // Allows exception handling

public class Fruit
{
    public static void main(String[] args)
    {
	// Declare and initialize three variables
	int numOranges = 5;
	int numApples = 10;
	int numFruit = 0;

	numFruit = numOranges + numApples;

	// Display result
	System.out.println("A totally fruity program");
	System.out.println("Total fruit is " + numFruit);
	// Code to delay ending the program
	System.out.println("(press Enter to exit) ");
	try
	    {
		System.in.read();
	    }
	catch (IOException e)
	    {
		return;
	    }
    }
}

	


