public class LinkedList
{
    // Default constructor - creates an empty list
    public LinkedList() {}

    // Constructor to create a list containing one object
    public LinkedList(Object item)
    {
	if(item != null)
	    current=end=start=new ListItem(item);  // item is start and end
    }

    // Constructor for a linked list from an array of objects
    public LinkedList(Object[] items)
    {
	if(items != null)
	    {
		// Add the items to the list
		for(int i = 0; i < items.length; i++)
		    addItem(items[i]);
		current = start;
	    }
    }

    // Add an item object to the list
    public void addItem(Object item)
    {
	ListItem newEnd = new ListItem(item); // Create a new listItem
	if(start == null)                     // Is the list empty
	    start = end = newEnd;             // Yes, so item is start and end
	else
	    {                                 // No, so...
		end.next = newEnd;            // Set next variable for old end
		end = newEnd;                 // Store new item as end
	    }
    }

    // Get the first Object in a list
    public Object getFirst()
    {
	current = start;
	return start == null ? null : start.item;
    }

    // Get the next object in the list
    public Object getNext()
    {
	if(current != null)
	    current = current.next; // Get the reference to the next item
	return current == null ? null : current.item;
    }

    private ListItem start = null;
    private ListItem end = null;
    private ListItem current = null;

    private class ListItem
    {
	// Constructor
	public ListItem(Object item)
	{
	    this.item = item;          // Store the item
	    next = null;               // Set next as end
	}

	// Return class name & object
	public String toString()
	{
	    return "ListItem " + item;
	}

	ListItem next;
	Object item;
    }
}
    
	
	
	
