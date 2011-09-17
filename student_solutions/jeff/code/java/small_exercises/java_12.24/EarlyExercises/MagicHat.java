import java.util.Random;

public class MagicHat
{
    static int maxRabbits = 5;
    static Random select = new Random();

    static private String[] rabbitNames = {"Floppsy", "Moppsy",
					   "Gnasher", "Thumper"};
    static private int[] rabbitNamesCount = new int[rabbitNames.length];

    // Constructor for a hat
    public MagicHat(final String hatName)
    {
	this.hatName = hatName;
	rabbits = new Rabbit[1+select.nextInt(maxRabbits)];

	for(int i = 0; i < rabbits.length; i++)
	    rabbits[i] = new Rabbit();
    }

    // String representation of a hat
    public String toString()
    {
	// Hat name first...
	String hatString = "\n" + hatName + " contains:\n";

	for(int i = 0; i < rabbits.length; i++)
	    hatString += "\t" + rabbits[i] + " ";
	return hatString;
    }

    private String hatName;   // Name of the hat
    private Rabbit rabbits[]; // Rabbits in the hat

    // Nested class to define a rabbit
    class Rabbit
    {
	// A name is a rabbit name from rabbitNames folled by an integer
	private String name;

	// Constructor for a rabbit
	public Rabbit()
	{
	    int index = select.nextInt(rabbitNames.length);  // Get random name
	                                                     // index
	    name = rabbitNames[index] + (++rabbitNamesCount[index]);
	}

	// String representation of a rabbit
	public String toString()
	{
	    return name;
	}
    }
}
    
