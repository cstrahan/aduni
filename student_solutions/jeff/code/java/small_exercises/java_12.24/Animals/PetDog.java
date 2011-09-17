public class PetDog extends Animal implements Cloneable
{
    // Constructor
    public PetDog(String name, String breed)
    {
	super("Dog");
	petFlea = new Flea("Max","circus flea");     // Initialize petFlea
	this.name = name;
	this.breed = breed;
    }

    // Rename the dog
    public void setName(String name)
    {
	this.name = name;
    }

    // Return the dog's name
    public String getName()
    {
	return name;
    }

    // Return the breed
    public String getBreed()
    {
	return breed;
    }

    // Return the flea
    public Flea getFlea()
    {
	return petFlea;
    }

    public void sound()
    {
	System.out.println("Woof.");
    }

    // Return a String for the pet dog
    public String toString()
    {
	return super.toString() + "\nIt's " + name + " the " + breed +
	    " & \n " + petFlea;
    }

    // Override inherited clone() to make it public
    public Object clone() throws CloneNotSupportedException
    {
	PetDog pet = (PetDog)super.clone();
	pet.petFlea = (Flea)petFlea.clone();

	return pet;
    }

    // Data types
    private Flea petFlea;
    private String name;
    private String breed;
}
	    
					      
					      
