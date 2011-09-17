/*
  Node.java
  Algorithms: PSet 5 - Problem 3
  Shyam Visweswaran
*/

public class Node
{
  Id id; // coordinates of node
  Id parent; // coordinates of parent node
  int value; // cell value
  int pathValue; // minimum cost path to current node
  
  public Node(int i, int j, int v)
  {
    id = new Id(i, j);
    parent = new Id(-1, -1);
    value = v;
    pathValue = -1;
  }
  
  public Id id()
  {
    return id;
  }
  
  public Id parent()
  {
    return parent;
  }
  
  public void setParent(Id id)
  {
    parent = id;
  }
  
  public int value()
  {
    return value;
  }
  
  public void setValue(int v)
  {
    value = v;
  }
  
  public int pathValue()
  {
    return pathValue;
  }
  
  public void setPathValue(int v)
  {
    pathValue = v;
  }
}


      
