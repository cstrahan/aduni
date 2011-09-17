public class TryExample
{
    public static void main(String[] args)
    {
	byte value = 1;
	for (int i=0 ; i<8 ; i++)
	    {
		value *= 2;
		System.out.println("Value is now " + value);
	    }
    }
}
