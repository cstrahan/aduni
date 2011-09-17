import java.io.*;

/**
   Encodes a file, using a particular cipher
   @author Jeffrey M. Radcliffe
   @version $Id: Encode.java,v 1.7 2001/01/19 13:14:43 jeff Exp $
*/
public class Encode
{
  public static void main(String[] args)
  {
    params = new String[3];

    // Copy the args to the params string
    try
    {
      if(args[0].equals("-d")) 
      {
        decodeFlag = true;
        System.out.println("Decode flag set.");
          params[0] = args[1];
          params[1] = args[2];
          params[2] = args[3];
      }
    
      else
      {
        params[0] = args[0];
        params[1] = args[1];
        params[2] = args[2];
      }
    }
    catch(ArrayIndexOutOfBoundsException e)
    {
    }

    // Determine the nature of the key
    createCipher(params[0]);      

    // Determine the output type
    try
    {
      // Check if there is an output argument
      File outFile = new File(params[2]);
      // Yes, set the file name
      out = new FileOutput(outFile);
    }
    catch(NullPointerException e)
    {
      // No, so create console output
      out = new ConsoleOutput();
    }

    // Determine the format type
    if(decodeFlag)
      // Decoding, make things more readable
      format = new StandardDecodeFormat();
    else
      // Encoding, create cryptic format
      format = new StandardEncodeFormat();
    
    // Create the ciphertext
    try
    {
      File myFile = new File(params[1]);
      InputStreamReader input = new InputStreamReader
        (new FileInputStream(myFile), "ASCII");

      int c;
      while(true)
      {
        c = input.read();       // Read a character from the text
        if(c == -1)             // Check for EOF
        {
          out.printCiphertext(format.getCipherText()); // Done! Print results
          break;
        }
        else if (c < 33)  {} // Char is out of range, ignore
        else
          format.formatOutput(cipher.encipher(c)); // Char is good, add to ciphertext
      }
    }
    catch(FileNotFoundException e)
    {
      System.out.println("Unable to find or open input file: " + args[1]);
      // System.err.println(e);
      return;
    }
    catch(IOException e)
    {
      System.out.println("IO Error on file " + args[1]);
      return;
    }
  } // end block main

  public static void createCipher(String keyType)
  {
    try
    {
      // Test to see if we have a valid integer key
      int test;
      test = Integer.parseInt(keyType);
      // TEST HERE FOR PROPER VALUES!!!
      cipher = new CaesarShiftCipher(test);
      return;
     }
    catch(NumberFormatException e)
    {
      // Not a valid integer value, send to Vignere cipher
      System.out.println("Key non-integer, attempting to create Vigenere cipher");
      cipher = new VigenereCipher(keyType);
      return;
    }
  } // end block createCipher

  public static boolean getDecodeFlag()
  {
    if(decodeFlag) return true;
    else return false;
  }
  
  private static CipherOutput out;    // The type of output
  private static CipherScheme cipher; // The method for ciphering
  private static FormatCipher format;
  private static boolean decodeFlag = false;
  private static String[] params;
  
} // End class encode

/**
   The basis for format types
*/
abstract class FormatCipher
{
  abstract void formatOutput(int c);
  abstract String getCipherText();
}

class StandardEncodeFormat extends FormatCipher
{
  public void formatOutput(int c)
  {
    cipherText += ((char)c);
    if(cluster == 4)
    {
      cipherText += " ";
      cluster = 0;
    }
    else
      cluster++;
  }

  public String getCipherText()
  {
    return cipherText;
  }
  
  private static String cipherText = "";
  private static int cluster = 0; // Formatting
}

class StandardDecodeFormat extends FormatCipher
{
  public void formatOutput(int c)
  {
    cipherText += ((char)c + " ");
  }

  public String getCipherText()
  {
    return cipherText;
  }

  private static String cipherText = "";
}

  
/**
   The basis for output types
*/
abstract class CipherOutput
{
  abstract void printCiphertext(String text);
}

/**
   Outputs cipher to a file
*/
class FileOutput extends CipherOutput
{
  FileOutput(File file)
  {
    outFile = file;
  }

  /**
     Opens a file and prints the ciphertext to it
     @param text A ciphertext
  */
  public void printCiphertext(String text)
  {
    try
    {
      cipherOutput = new PrintWriter(new
        FileOutputStream(outFile), true);
      cipherOutput.println(text);
    }
    catch(FileNotFoundException e) {
      System.out.println("Cannot open file " + outFile);
    }
    catch(IOException e) {
      System.out.println("IOException with " + outFile);
    }
  }
  private File outFile;
  private PrintWriter cipherOutput;
} // end class FileOutput

/**
   Outputs cipher to the console
*/
class ConsoleOutput extends CipherOutput
{
  /**
     Prints the cipher
     @param text the ciphertext
  */
  public void printCiphertext(String text)
  {
    System.out.println("|========================= BEGIN CODE =========================|\n"
                       + text +
                       "\n|=========================  END CODE  =========================|");
  }
}

/**
   Abstract class for ciphers
*/
abstract class CipherScheme
{
  abstract int encipher(int c); // enciphers a character
}

/**
   A basic Caesar-shift cipher
*/
class CaesarShiftCipher extends CipherScheme
{
  CaesarShiftCipher(int key)
  {
    if(Encode.getDecodeFlag())
    {
      this.key = (key * -1);
    }
    else
    {
      this.key = key;
      System.out.println("Using Caesar Shift cipher scheme.");
    }
    
  }
  /**
     A basic Caesar-shift cipher
     @param shift the number of ASCII positions to shift
     by (modulo the range of printed characters)
     @return An enciphered character
  */
  public int encipher(int shift)
  {
    int temp = shift + key;
    if(temp >= 127)
      temp = 33 + ((temp - 127) % 95);
    char cipherChar = (char) temp;
    return cipherChar;
  }
  private static int key; 
} // end class CaesarShiftCipher

/**
   The Vigenere cipher
   A more complex cipher, using a key word.
   Is there an easy way to code the decoding of this?
*/
class VigenereCipher extends CipherScheme
{
  VigenereCipher(String key)
  {
    // First, test to see if we have a good key
    if(keyTest(key))
    {
      // It's good, so construct the object
      keyLength = key.length() - 1;
      if(Encode.getDecodeFlag())
      {
        decode = true;
      }
      System.out.println("Key is valid. " +
                         "Vigenere cipher scheme created");
    }
    else
    {
      System.out.println("Key is not valid. Must be printable characters only.");
      System.exit(0);
    }
  }

  /**
     Tests for validity of the key
  */
  private static boolean keyTest(String testKey)
  {
    // Convert the string into an array of chars
    char[] test = new char[testKey.length()];
    testKey.getChars(0, testKey.length(), test, 0);

    // Conver the array of chars to an array of integers, checking for validity
    key = new int[testKey.length()];
    for(int i = 0; i < testKey.length() ; i++)
    {
      int x = (int)test[i];
      if(x < 32 || x > 127) return false;
      else key[i]= x;
    }
    // Key is good.
    return true;
  }
  
  /**
     Enciphers the text
     @param int The character to be enciphered
     @return A ciphered character
  */
  public int encipher(int shift)
  {
    if(this.decode)
    {
      // Decode a vigenere cipher
      int temp = shift - key[count];
      if(temp <= 32)
        temp = 127 + (temp - 33) % 95;
      char cipherChar = (char) temp;
//              System.out.println("char(" + shift + ") - key(" + key[count] +
//         ") -> " + temp + " (" + (char)temp + ")");
      if(count++ == keyLength) count = 0;
      return cipherChar;
    }
    else
    {
      // Encode a vigenere cipher
      int temp = shift + key[count];
      if(temp >= 127)
        temp = 33 + ((temp - 127) % 95);
      char cipherChar = (char) temp;
//        System.out.println("char(" + shift + ") + key(" + key[count] +
//         ") -> " + temp + " (" + (char)temp + ")");
      if(count++ == keyLength) count = 0;
      return cipherChar;
    }
  } 
  private static int count = 0;
  private static int[] key;
  private static int keyLength;
  private static boolean decode = false;

} // end class VigenereCipher
