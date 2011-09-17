import java.io.*;

public class FormattedInput
{
    // Method to read an int value...
    public int intRead()
    {
	try
	    {
		for (int i = 0; i < 5 ; i++)
		    {
			if(tokenizer.nextToken() == tokenizer.TT_NUMBER)
			    return (int)tokenizer.nval;
			else
			    {
				System.out.println("Incorrect input: "
						   + tokenizer.sval
						   + " Return-enter an integer");
				continue;
			    }
		    }
		System.out.println("Five failures reading an int value" +
				   " - program terminated");
		System.exit(1);
		return 0;
	    }
	catch(IOException e)
	    {
		System.out.println(e);
		System.exit(1);
		return 0;
	    }
    }
		    
    // Plus methods to read various other data types
    public String stringRead()
    {
	try
	    {
		for(int i =0; i < 5; i++)
		    {
			int tokenType = tokenizer.nextToken();
			if(tokenType == tokenizer.TT_WORD || tokenType == '\"')
			    return tokenizer.sval;
			else if(tokenType == '!')
			    return "!";
			else
			    {
				System.out.println
				    ("Incorrect input. Return-enter a string between double quotes");
				continue;
			    }
		    }
		System.out.println("Five failures reading a string" +
				   " - program terminated");
		System.exit(1);
		return null;
	    }
	catch(IOException e)
	    {
		System.out.println(e);
		System.exit(1);
		return null;
	    }
    }

    // Object to tokenize input from the standard input stream
    private StreamTokenizer tokenizer = new StreamTokenizer
	(new InputStreamReader(System.in));
}
