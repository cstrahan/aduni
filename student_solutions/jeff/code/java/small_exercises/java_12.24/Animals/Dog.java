public class Dog extends Animal
{
    // constructors for a Dog object
    public Dog(String aName)
    {
	super("Dog");
	name = aName;
	breed = "Unknown";
    }

    public Dog(String aName, String aBreed)
    {
	super("Dog");
	name = aName;
	breed = aBreed;
    }

    // Bark
    public void sound()
    {
	System.out.println("Woof     Woof");
    }

    // Present a dog's details as a string
    public String toString()
    {
	return super.toString() + "\nIt's " + name + " the " + breed;
    }

    private String name;
    private String breed;
}
