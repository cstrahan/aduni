public class StringCharacters
{
    public static void main(String[] args)
    {
	// Text string to be analyzed
	String text = "To be or not to be, that is the question;"
	    + "Whether 'tis nobler in the mind to suffer"
	    + " the slings and arrows of outrageous fortune,"
	    + " or to take arms against a sea of troubles,"
	    + " and by opposing end them.";

	int spaces = 0;
	int vowels = 0;
	int letters = 0;

	// Analyze all the characteres in the string
	int textlength = text.length();    // get string length
	for(int i = 0; i < textlength; i++)
	    {
		// check for voewls
		char ch = Character.toLowerCase(text.charAt(i));
		if(ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u')
		    vowels++;

		// check for letters
		if(Character.isLetter(ch))
		    letters++;

		// check for spaces
		if(Character.isWhitespace(ch))
		    spaces++;
	    }
	System.out.println("The text contained vowels:       " + vowels + "\n" +
			   "                   consonants:   " + ( letters - vowels) + "\n" +
			   "                   spaces:       " + spaces);
    }
}
