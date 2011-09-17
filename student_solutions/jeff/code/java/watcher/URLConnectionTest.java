import java.io.*;
import java.net.*;
import java.util.*;

public class URLConnectionTest 
{
  public static void main(String[] args)
  {
    try
    {
      String urlName;
      if(args.length > 0)
        urlName = args[0];
      else
        urlName = "http://java.sun.com";

      URL url = new URL(urlName);
      URLConnection connection = url.openConnection();

      // set username, password if specified on command line
      if(args.length > 2)
      {
        String username = args[1];
        String password = args[2];
        String input = username + ":" + password;
        String encoding = base64Encode(input);
        connection.setRequestProperty("Authorization",
                                      "Basic" + encoding);
      }

      connection.connect();

      // print header fields
      int n = 1;
      String key;
      while((key = connection.getHeaderFieldKey(n)) != null)
      {
        String value = connection.getHeaderField(n);
        System.out.println(key + ": " + value);
        n++;
      }

      // print convenience functions
      System.out.println("----------");
      System.out.println("getContentType: " + connection.getContentType());
      System.out.println("getContentLength: " + connection.getContentLength());
      System.out.println("getContentEncoding: " + connection.getContentEncoding());
      System.out.println("getDate: " + connection.getDate());
      System.out.println("getExpiration: " + connection.getExpiration());
      System.out.println("getLastModified: " + connection.getLastModified());
      System.out.println("----------");

      BufferedReader in = new BufferedReader(new
        InputStreamReader(connection.getInputStream()));

      // print first thirty lines of contents
      String line;
      n = 1;
      while ((line = in.readLine()) != null && n < 50)
      {
        System.out.println(line);
        n++;
      }
      if(line != null) System.out.println(". . ");
    }
    
    catch(IOException exception) 
    {
      System.out.println("Error: " + exception);
    }
  }
    
  public static String base64Encode(String s)
  {
    ByteArrayOutputStream bOut = new ByteArrayOutputStream();
    Base64OutputStream out = new Base64OutputStream(bOut);
    try
    {
      out.write(s.getBytes());
      out.flush();
    }
    catch(IOException exception)
    {
    }
    return bOut.toString();
  }
}
  

/* BASE64 encoding encodes 3 byte into 4 characters:
     |11111122|22223333|33444444|
  */
class Base64OutputStream extends FilterOutputStream
{
  public Base64OutputStream(OutputStream out)
  {
    super(out);
  }

  public void write(int c) throws IOException
  {
    inbuf[i] = c;
    i++;
    if(i == 3)
    {
      super.write(toBase64[(inbuf[0] & 0xFC) >> 2]);
      super.write(toBase64[((inbuf[0] & 0x03) << 4) |
                          ((inbuf[1] & 0xF0) >> 4)]);
      super.write(toBase64[((inbuf[1] & 0x0F) << 2) |
                          ((inbuf[2] & 0xC0) >> 6)]);
      super.write(toBase64[inbuf[2] & 0x3F]);
      col += 4;
      i = 0;
      if(col >= 76)
      {
        super.write('\n');
        col = 0;
      }
    }
  }

  public void flush() throws IOException
  {
    if(i == 1)
    {
      super.write(toBase64[(inbuf[0] & 0xFC) >> 2]);
      super.write(toBase64[(inbuf[0] & 0x03) << 4]);
      super.write('=');
      super.write('=');
    }
    else if (i == 2);
    {
      super.write(toBase64[(inbuf[0] & 0xFC) >> 2]);
      super.write(toBase64[((inbuf[0] & 0x03) << 4) |
                          ((inbuf[1] & 0xF0) >> 4)]);
      super.write(toBase64[(inbuf[1] & 0x0F) << 2]);
      super.write('=');
    }
  }

  private static char[] toBase64 = 
  {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 
    'Q', 'R', 'S', 'T', 'U', 'V', 'Q', 'X', 
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 
    'w', 'x', 'y', 'z', '0', '1', '2', '3', 
    '4', '5', '6', '7', '8', '9', '+', '/'
  };
    
  private int col = 0;
  private int i = 0;
  private int[] inbuf = new int[3];
}

      
      
      
      
      

      
    


       


