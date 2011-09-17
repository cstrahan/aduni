public class ZeroDivideException extends Exception
{
    private int index = -1;    //Index of array element causing error

    // Default constructor
    public ZeroDivideException(){ }

    // Standard constructor
    public ZeroDivideException(String s)
    {
	super(s);                         // call the base constructor	
    }

    public ZeroDivideException(int index)
    {
	super("/ by zero");               // call the base constructor
	this.index = index;
    }

    // Get the array index value for the error
    public int getIndex()
    {
	return index;                    // Return the index value
    }
}
