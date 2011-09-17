public class NewLetterCheck
{
    public static void main(String[] args)
    {
	char symbol = 'A';
	symbol = (char)(128.0*Math.random());  // Generate a random character
	
	if(Character.isUpperCase(symbol))
	    System.out.println("You have the capital letter " + symbol);
	else
	    if(Character.isLowerCase(symbol))
		System.out.println("You have the lowercase letter " + symbol);
	else
	    System.out.println("The code is not a letter");
    }
}
