import java.io.*;

class PhoneNumber implements Serializable
{
    // Read a phone number from the keyboard
    public static PhoneNumber readNumber()
    {
	FormattedInput in = new FormattedInput();

	// Read in the area code
	System.out.println("\nEnter the area code:");
	String area = Integer.toString(in.intRead());

	// Read the number
	System.out.println("Enter the local code:");
	String number = Integer.toString(in.intRead());

	System.out.println("Enter the number:");
	number += " " + Integer.toString(in.intRead());

	return new PhoneNumber(area,number);
    }
   
    public PhoneNumber(String areacode, String number)
    {
	this.areacode = areacode;
	this.number = number;
    }

    public String toString()
    {
	return areacode + ' ' + number;
    }

    private String areacode;
    private String number;
}

    
	
	
