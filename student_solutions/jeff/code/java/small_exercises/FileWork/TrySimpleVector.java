
/**
 * TrySimpleVector
 *
 *
 * Created: Wed Dec 27 15:44:19 2000
 *
 * @author 
 * @version
 */
import java.util.*;

public class TrySimpleVector
{
    public static void main(String[] args)
    {
	Vector names = new Vector();
	String[] firstnames= { "Jack", "Jill", "Joan", "Jeremiah",
			       "Josephine", "Julio", "Juanita"};

	for(int i = 0; i < firstnames.length; i++)
	    names.add(firstnames[i]);

	for(int i = 0 ; i < names.size() ; i++)
	    System.out.println((String)names.get(i));
    }
}

		
       
 
	
 
