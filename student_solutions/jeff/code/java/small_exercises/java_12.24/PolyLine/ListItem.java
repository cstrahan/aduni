class ListItem
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

	
	    
