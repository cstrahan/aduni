interface PriorityQueue
{
  /**
     Add an Object to the queue
  */
  public void insert(Comparable a);

  /**
     Removes the maximum object from the queue
  */
  public Comparable removeMax();

  /**
    Returns true iff queue is empty
  */
  public boolean empty();

  /**
     Returns the number of objects int the queue
  */
  public int length();
}

     
