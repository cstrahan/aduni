

public class LetterCheck
{
    public static void main(String[] args)
    {
	char symbol = 'A';
	symbol = (char)(128.0*Math.random());  // Generate a random character
	
	System.out.println("The random character is " + symbol);

	if(symbol >= 'A')                      // Greater than A ?
	    {
		if(symbol <= 'Z')              // yes, and Z or less?
		    // true, it's a capital
		    System.out.println("You have a capital letter " + symbol);
		else
		    if(symbol >= 'a')
			if(symbol <= 'z')
			    // then it's a lowercase
			    System.out.println("You have a lowercase letter " + symbol);
			else
			    System.out.println("The code is greater than a but it's not a letter");
		    else System.out.println("The code is less than a but it's not a letter");
	    }
	else System.out.println("The code is less than A so it's not a letter");
    }
}
