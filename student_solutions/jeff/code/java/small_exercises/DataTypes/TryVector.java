import java.util.*;
import java.io.*;

public class TryVector
{
    public static void main(String[] args)
    {
	Person aPerson;
	Crowd filmCast = new Crowd();

	// Populate the crowd
	for( ; ; )
	    {
		aPerson = readPerson();
		if(aPerson == null)
		    break;
		filmCast.add(aPerson);
	    }

	int count = filmCast.size();
	System.out.println("You added " + count +
			   (count == 1 ? " person" : " people") + " to the cast.\n");

	filmCast.sort();
	
	// Show who is in the cast using an iterator
	Iterator thisLot = filmCast.iterator();

	while(thisLot.hasNext())
	    System.out.println(thisLot.next());
    }

    static public Person readPerson()
    {
	FormattedInput in = new FormattedInput();

	// Read in the first name and remove blanks front and back
	System.out.println("\nEnter first name or ! to end:");
	String firstName = in.stringRead().trim();
	
	if(firstName.charAt(0) == '!')
	    return null;

	// Read in the surname, also trimming blankss
	System.out.println("Enter surname:");
	String surname = in.stringRead().trim();
	return new Person(firstName, surname);
    }
}
	    
