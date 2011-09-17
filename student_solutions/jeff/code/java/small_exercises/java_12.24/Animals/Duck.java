public class Duck extends Animal
{
    // constructors for a Duck object
    public Duck(String aName)
    {
	super("Duck");
	name = aName;
	breed = "Unknown";
    }

    public Duck(String aName, String aBreed)
    {
	super("Duck");
	name = aName;
	breed = aBreed;
    }

    // Quack
    public void sound()
    {
	System.out.println("Quack.");
    }

    // Lay eggs
    public void layEgg()
    {
	System.out.println("Egg laid");
    }
    
    // Present a Duck's details as a string
    public String toString()
    {
	return super.toString() + "\nIt's " + name + " the " + breed;
    }

    private String name;
    private String breed;
}
