/**
   PS-2 Problem 1 - Encode reads a character file line by line, shifts
   each character byte an integer amount and prints it out line by line
   to another file or to the console.
   @author shyam Visweswaran
*/
import java.util.*;
import java.io.*;

public class Encode
{
  // variables
  static final int minChar = 33; 
  static final int maxChar = 126;
  static final int maxShift = maxChar - minChar;
  private BufferedReader fileIn = null;
  private PrintStream fileOut = System.out; // default output is to console
  private int shift; // shift = input key

  /**
   This is the main routine
   @param args key infile [outfile]
  */
  public static void main(String[] args)
  {
    try
    {
      Encode myEncode = new Encode(args);
      myEncode.doIOStuff();
    }
    catch (EncodeException exception)
    { // print out to error in case system out is redirected
      System.err.println(exception.getMessage());
    }
    catch (IOException exception)
    {
      System.err.println(exception.getMessage());
    }
  }
  
  /**
     Constructor
  */
  public Encode(String[] args) throws EncodeException, IOException
  { // less than 2 command line args
    if (args.length < 2)
    {
      throw new EncodeException("Not enough parameters! Use java Encode key infile [outfile]");
    } // more than 3 command line args
    else if (args.length > 3)
    {
      throw new EncodeException("Too many parameters! Use java Encode key infile [outfile]");
    } // key is out of range
    else if ((shift = Integer.parseInt(args[0])) < 0 || shift > maxShift)
    {
      throw new EncodeException("Key " + args[0] + " is out of range. Use an integer between 0 and " + maxShift);
    }
    else
    {
      fileIn = new BufferedReader(new FileReader(args[1]));
      // if third arg is available set output to that file
      if (args.length == 3) fileOut = new PrintStream(new FileOutputStream(args[2]));
    }
  }
  
  private void doIOStuff() throws EncodeException, IOException
  {
    char charIn;
    char charOut;
    String stringIn;
    StringBuffer stringOut = new StringBuffer("");
    // read one line at a time from input file
    while ((stringIn = fileIn.readLine()) != null)
    {
      stringOut.setLength(0);
      for (int i = 0; i < stringIn.length(); i++)
      {
        charIn = stringIn.charAt(i);
        if (charIn == '\t' || charIn == ' ')
        { // leave space and tab unchanged
          charOut = charIn;
        }
        else if (charIn < minChar || charIn > maxChar)
        { // not an ASCII character
          throw new EncodeException("This file has non-ASCII characters that are not readable");
        }
        else
        { // shift character and if necessary wrap around;
          // casting needed since shift is an integer and others are char
          if (charIn + shift > maxChar) charOut = (char)(charIn + shift - maxShift);
          else charOut = (char)(charIn + shift);
        } // end if
        // create output line
        stringOut.append(charOut);
      } // end for
      // write output line with a carriage return
      fileOut.println(stringOut);
    } // end while
    // close file if writing to a file
    if (fileOut != System.out) fileOut.close();
  }
  
  /**
     Errors from Encode; has 2 constructors
  */
  class EncodeException extends IOException
  {
    public EncodeException()
    {
      super();
    }
    public EncodeException(String string)
    {
      super(string);
    }
  }
}

    
    
      
          
        
