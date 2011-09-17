import java.util.LinkedList;

public class PQueue implements PriorityQueue
{/** Problem 8, problem set 1, Java/OOP.
 *   Create a simple priority queue using the interfaces PriorityQueue and
 *   Comparable.
 *   @author Tom Hickerson, January 2001.
 */
    LinkedList pqList = new LinkedList();


    public void insert(Comparable a)
    {/** Accepts and sorts a Comparable type into a linked list;
     *   higher ordered items are placed first on the list, 
     *   so that removeMax() can easily "pop" the largest value 
     *   "off" the list.
     *   @param a The comparable object.
     *   @param b Counter variable, used to place the values int the right place. 
     */
	int b = 0;
	if (pqList.isEmpty()) pqList.add(b,a);
	else while (b < pqList.size())
	    {	
		if (((Comparable)pqList.get(b)).compareTo(a) < 0)
		    {
			int c = pqList.size() + 1;
		        pqList.add(b,a);
      			b = c;
		    }
		else b++;
	    }

    }

    public Comparable removeMax()
    {/** Removes the first item on the list, which is sorted already.
     */
	if (!(pqList.isEmpty())) return (Comparable)pqList.removeFirst();
	else return null;
    }

    public boolean empty()
    {/** Returns whether or not the queue is empty.
     */
	return pqList.isEmpty();
    }

    public int length()
    {/** Returns the length of the queue.
     */
	return pqList.size();
    }

    /** Questions for problem 8:
     *  The best solution for the abstrction problem described at the end of the problem
     *  set is to try and get around the function call to compareTo altogether.  CompareTo
     *  will insert but cannot accept different data structures.  A solution might be
     *  a rewrite of compareTo which specifically accepts one type and another type
     *  (using a version of overloading).
     *  The TreeMap solution would work, because it is not attached to the data type.
     */ 
}
