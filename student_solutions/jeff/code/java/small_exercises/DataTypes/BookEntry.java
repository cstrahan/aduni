import java.io.*;

class BookEntry implements Serializable
{
    public BookEntry(Person person, PhoneNumber number)
    {
	this.person = person;
	this.number = number;
    }

    public Person getPerson()
    { return person; }

    public PhoneNumber getNumber()
    { return number; }

    public String toString()
    {
	return person.toString() + '\n' + number.toString();
    }

    // Read an entry from the keyboard
    public static BookEntry readEntry()
    {
	return new BookEntry(Person.readPerson(), PhoneNumber.readNumber());
    }

    private Person person;
    private PhoneNumber number;
}

    
    
