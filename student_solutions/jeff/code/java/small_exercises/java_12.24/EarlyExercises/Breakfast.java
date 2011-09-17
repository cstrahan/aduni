public class Breakfast
{
    public static void main(String[] args)
    {
	// Generate a random number
	int item = 0;
	
	item = (int)(6.0*Math.random());

	System.out.println("What are you having for breakfast today?");


	// Pick Breakfast accordingly
	switch(item)
	    {
	    case 0:
		System.out.println("\tOatmeal");
		break;
	    case 1:
		System.out.println("\tBagels");
		break;
	    case 2:
		System.out.println("\tToast");
		break;
	    case 3:
		System.out.println("\tPancakes");
		break;
	    case 4:
		System.out.println("\tWaffles");
		break;
	    case 5:
		System.out.println("\tEggs and Bacon");
	    }
	System.out.println("Yum!");
    }
}
