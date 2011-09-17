public class ListPoint
{
    // Constructor
    public ListPoint(Point point)
    {
	this.point = point; // Store point reference
	next = null;    // Set next ListPoint as nul
    }

    // Set the point to the next ListPoint
    public void setNext(ListPoint next)
    {
	this.next = next; // Store the next ListPoint
    }

    // Get the next point in the list
    public ListPoint getNext()
    {
	return next;   // Return the next ListPoint
    }

    // Return String representation
    public String toString()
    {
	return "(" + point + ")";
    }

    // Data
    private ListPoint next;
    private Point point;
}
