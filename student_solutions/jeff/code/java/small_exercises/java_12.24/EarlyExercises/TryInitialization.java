public class TryInitialization
{
    static int[] values = new int[10];

    // Initialization block
    // static
    {
	System.out.println("Running initialization block.");
	for(int i=0; i < values.length ; i++)
	    values[i] = (int)(100.0*Math.random());
    }

    // List values in the array for an object
    void listValues()
    {
	System.out.println();
	for(int i=0 ; i < values.length ; i++)
	    System.out.print(" " + values[i]);
	System.out.println();
    }

    public static void main(String[] args)
    {
	TryInitialization example = new TryInitialization();
	System.out.println("\nFirst object:");
	example.listValues();

	example = new TryInitialization();
	System.out.println("\nSecond object:");
	example.listValues();

	example = new TryInitialization();
	System.out.println("\nThird object:");
	example.listValues();

	
    }
}
