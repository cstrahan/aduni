import java.io.*;

public class Decode
{
  public static void main(String[] args)
  {
    try
    {
    key = Integer.parseInt(args[0]);
    System.out.println(args[2]);
    }
    catch(ArrayIndexOutOfBoundsException e)
    {
      System.out.println("Usage: java Encode <key> <input file> [<output file>]\n");
      System.exit(0);
    }

    try
    {
      
      File myFile = new File(args[1]);
      InputStreamReader input = new InputStreamReader
        (new FileInputStream(myFile), "ASCII");
      
      int c;
      while(true)
      {
        c = input.read();
        if(c == -1)
        {
          System.out.println();
          break;
        }
        else if (c < 32 || c > 127)
          System.out.print("");
        else
          display(encipher(c));
      }
    }
    catch(FileNotFoundException e)
    {
      System.err.println(e);
      return;
    }
    catch(IOException e)
    {
      System.err.println(e);
      return;
    }
  }

  public static int encipher(int shift)
  {
    int temp = shift - key;
    if(temp >= 127)
      temp = 32 + (temp - 127);
    char output = (char) temp;
    return output;
  }

  public static void display(int c)
  {
    System.out.print((char)c + " ");
    
  }
  
  private static int key;
}
