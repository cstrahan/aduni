public class Cat extends Animal
{
    // constructors for a Cat object
    public Cat(String aName)
    {
	super("Cat");
	name = aName;
	breed = "Unknown";
    }

    public Cat(String aName, String aBreed)
    {
	super("Cat");
	name = aName;
	breed = aBreed;
    }

    // Miaow
    public void sound()
    {
	System.out.println("Miiiaaaow");
    }

    // Present a cat's details as a string
    public String toString()
    {
	return super.toString() + "\nIt's " + name + " the " + breed;
    }

    private String name;
    private String breed;
}
