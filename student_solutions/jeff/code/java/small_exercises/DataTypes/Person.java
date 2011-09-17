import java.io.*;

public class Person implements Comparable, Serializable
{
    // Constructor
    public Person(String firstName, String surname)
    {
	this.firstName = firstName;
	this.surname = surname;
    }

    // Read a person from the keyboard
    public static Person readPerson()
    {
	FormattedInput in = new FormattedInput();
	// Read in the first name and remove blanks front and back
	System.out.println("\nEnter first name:");
	String firstName = in.stringRead().trim();

	// Read in the surname and remove blanks front and back
	System.out.println("\nEnter surname:");
	String surname = in.stringRead().trim();

	return new Person(firstName, surname);
    }
    
    public String toString()
    {
	return firstName + " " + surname;
    }

    public boolean equals(Object person)
    {
	return compareTo(person) == 0;
    }

    public int hashCode()
    {
	return 7*firstName.hashCode() + 13*surname.hashCode();
    }

    // Compare Person objects
    public int compareTo(Object person)
    {
	int result = surname.compareTo(((Person)person).surname);
	return result == 0 ? firstName.compareTo(((Person)person).firstName) : result;
    }

    private String firstName;
    private String surname;
}
    
