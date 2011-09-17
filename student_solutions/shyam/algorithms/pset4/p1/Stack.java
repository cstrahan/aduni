/*
  Stack.java - stack for Points implemented with LinkedLists
  Algorithms: PSet4 - Problem 1
  Shyam Visweswaran
*/
import java.util.*;

public class Stack
{
  LinkedList stack;
  
  public Stack()
  {
    stack = new LinkedList();
  }
  
  public void push(Point p)
  {
    stack.add(p);
  }
  
  public void pop()
  {
    stack.removeLast();
  }
  
  public Point top()
  {
    return (Point)stack.getLast();
  }
  
  public Point below()
  { // one below the top
    return (Point)stack.get(size() - 2);
  }
  
  public Point get(int i)
  {
    return (Point)stack.get(i);
  }
  
  public int size()
  {
    return stack.size();
  }
  
  public boolean empty()
  {
    if (size() == 0) return true;
    else return false;
  }
}
