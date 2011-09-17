import java.io.*;

/**
 *  Problem 1, Problem Set 2, Java/OOP.
 *  Write a file Encode that exemplifies exception handling and input/output
 *  issues.
 *  @author Tom Hickerson, January 2001.
 *  @param io The error exception that catches an incorrect number after Encode.
 */

class Encode
{
    public static void main(String[] args)
    {
	int numkey;
	String infile;
	String outfile;
	String charkey;
	if (args.length < 2)
	    {
		System.out.println("ERROR: Incorrect input");
		System.exit(0);
	    }
	try
	    {
		numkey = Integer.parseInt(args[0]);
	  	infile = args[1];
		
		if (args.length==2) outfile = null;
		else outfile = args[2];
		ReadAndEncrypt(numkey,infile,outfile);
	    }
	catch(NumberFormatException io)
	    {
		System.out.println("Enter a number:" + io);
		System.exit(0);
	    }
    }
    /** The only class int the program: ReadAndEncrypt.
     *  @param num The number key for the Ceasar cypher.
     *  @param infile The string with the source file name.
     *  @param outfile The string with the write file name.
     *  @param io1 The exception message that halts the program.
     *  @param temp As per the PSet, the value of the character shifted num times.
     *  @param ans The array of chars that is sent to shift num times.
     *  @param x The String which is read into the encryption shifter.
     *  @param answer The String which is returned either to the screen or the write file.
     */
    public static void ReadAndEncrypt(int num, String infile, String outfile)
    {
	try
	    {
		BufferedReader infil = 
		    new BufferedReader(new FileReader(infile));
		String x;
		String answer = "";
		while ((x = infil.readLine()) != null)
		    {
      			int i = x.length();
			char[] ans = new char[i];
			for (int j = 0; j < i; j++)
			    {
				ans[j] = x.charAt(j);
			    }
			for (int k = 0; k < i; k++)
			    {
				int temp = (int) ans[k] + num;
				if (temp >= 127) temp = 32 + (temp - 127);
				answer += (char) temp;
			    }
			answer += "\n";
		    }
		if (outfile==null) System.out.println(answer);
      		else
		    {
			FileWriter f = new FileWriter(outfile);
			BufferedWriter bf = new BufferedWriter(f);
			PrintWriter pbf = new PrintWriter(bf);
			pbf.print(answer);
			pbf.close();
		    }
	    }
	catch(IOException io1)
	    {
		System.out.println("Incorrect input file:" + io1);
		System.exit(0);
	    }
    }    
}
