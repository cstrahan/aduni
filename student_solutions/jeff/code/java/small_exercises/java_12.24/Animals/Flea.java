public class Flea extends Animal implements Cloneable
{
    // Constructor
    public Flea(String aName, String aSpecies)
    {
	super("Flea");     // Pass the type to the base
	name = aName;      // Supplied name
	species = aSpecies; // Supplied species
    }

    // Change the flea's name
    public void setName(String aName)
    {
	name = aName;      // Change to the new name
    }

    // Return the flea's name
    public String getName()
    {
	return name;
    }

    // Return the species
    public String getSpecies()
    {
	return species;
    }

    public void sound()
    {
	System.out.println("Psst");
    }

    // Present a flea's details as a string
    public String toString()
    {
	return super.toString() + "\nIt's " + name + " the " + species;
    }

    // Override inherited clone() to make it public
    public Object clone() throws CloneNotSupportedException
    {
	return super.clone();
    }

    // Data types
    private String name;
    private String species;
}
	
