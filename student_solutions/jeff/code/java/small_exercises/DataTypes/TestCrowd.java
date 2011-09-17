import java.util.*;
import java.io.*;

public class TestCrowd
{
    public static void main(String[] args)
    {
	String string1 = "Foo";
	String string2 = "Bar";
	String string3 = "Baz";

	Person bob = new Person(string1,string2);
	System.out.println(bob);
	
	Person joe = new Person(string2,string3);
	Person jim = new Person(string3,string1);
	Crowd myCrowd = new Crowd();



	System.out.println("All things defined");	
	myCrowd.add(bob);

	Iterator myIterator = myCrowd.iterator();
	while(myIterator.hasNext())
	    System.out.println(myIterator.next());
	/**
	myCrowd.add(joe);

	while(myIterator.hasNext())
	System.out.println(myIterator.next()); */
    }
}

