import java.util.*;             // Needed for LinkedList

/**
   PQueue.java
   I am using a LinkedList for underlying representation of the PQueue.

   @author Jeffrey M. Radcliffe
   @version 0.1
*/
public class PQueue implements PriorityQueue
{
  /**
     Inserts an item into the queue, and sorts at the same time.
     The largest item will be added at the end of the queue
     @param a The item to be inserted
  */
  public void insert(Comparable a)
  {
    // Will sort, placing largest objects first
    for(int i = 0; i < q.size(); i++)
    {
      System.out.print("Sorting ... ");
      // Is our object larger than the object indexed at i?
      if(a.compareTo(q.get(i)) == -1)
      {
        // no, so insert it into the current index
        q.add(i , a);
        return;
      }
      System.out.println(a + " is larger than " + q.get(i));
    }
    // At the end, must be the largest!
      q.addLast(a);
  }

  /**
     Removes the largest item from the PQueue
     @return The largest Comparable from the queue
  */
  public Comparable removeMax()
  {
    System.out.println("Removing largest object ... ");
    Comparable temp = (Comparable)q.removeLast();
    return temp;
  }

  /**
     Tests to see if the PQ is empty
     @return <code>true</code> if empty, <code>false</code> if not
  */
  public boolean empty()
  {
    if(q.size() == 0)
      return true;
    else
      return false;
  }
  public int length()
  {
    return q.size();
  }

  /**
     Gets the contents of the PQueue.
     @return the contents of the PQueue; [<item 1> <item 2> ... <item n> ]
  */
  public String contents()
  {
    String results = "[ ";
    Iterator iter = q.iterator(); // Create a new iterator ...
    while(iter.hasNext())         // And output the contents
      results += iter.next() + " ";
    return results + "]";
  }

  // field
  private LinkedList q = new LinkedList();
}
