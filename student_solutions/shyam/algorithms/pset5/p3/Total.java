/*
  Carnival.java - uses Node.java, Id.java
  Algorithms: PSet 5 - Problem 3
  Shyam Visweswaran
*/
public class Carnival
{
  static Node[][] table; // 2-D array of nodes representing squares of the grid
  static Node[] path; // nodes int the minimum cost path
  static int size; // size of array
  
  public static void main(String[] args)
  { // single integer argument is the size of the array
    if ((args.length < 1) || (args.length > 1))
      System.out.println("Error. Enter one integer argument.");
  
    size = Integer.parseInt(args[0]);
    table = new Node[size][size];

    // populate node table with random values between 1 and n   
    for (int i = 0; i < size; i++)
      for (int j = 0; j < size; j++)
        table[i][j] = new Node(i, j, (int)(Math.random() * size) + 1);

    computePathValues(); // calculate the minimum cost for each node
    getMinPath(); // get the coordinates of the nodes in the minimum cost path

    // print out input array with the computed minimum costs for each node
    System.out.println("Input grid... Legend: cell value(minimum cost)");
    for (int i = 0; i < size; i++)
    {
      for (int j = 0; j < size; j++)
      {
        System.out.print(table[i][j].value() + "(" + table[i][j].pathValue() + ") ");
      }
      System.out.println();
    }

    // print out nodes in the minimum cost path, right to left
    System.out.println();
    System.out.println("Minimum cost path... Legend: coordinates[cell value]");
    System.out.print("Start");
    for (int i = (size - 1); i >= 0; i--)
      System.out.print(" -> " + path[i].id().toString() + "[" + path[i].value() + "]");
    System.out.println();
  }
    
  public static void computePathValues()
  { // note j is the outer loop since we need all the values of j-1 column before going on to column j
    for (int j = 0; j < size; j++)
    {
      for (int i = 0; i < size; i++)
      {
        Node current = table[i][j];
        if (j == 0) current.setPathValue(current.value()); // 0th column
        else 
        { // modulo could be used here with some tweaking
          Node min;
          if (i == 0) min = getMin(table[size-1][j-1], table[i][j-1], table[i+1][j-1]);
          else if (i == (size - 1))  min = getMin(table[i-1][j-1], table[i][j-1], table[0][j-1]);
          else min = getMin(table[i-1][j-1], table[i][j-1], table[i+1][j-1]);
          current.setPathValue(current.value() + min.pathValue()); // set the minimum cost path so far
          current.setParent(min.id()); // set parent of current node
        }
      }
    }
  }

  // utility for computing which node of 3 has the minimum cost so far
  public static Node getMin(Node a, Node b, Node c)
  {
    Node min = a;
    if (b.pathValue() < min.pathValue()) min = b;
    if (c.pathValue() < min.pathValue()) min = c;
    return min;
  }

  // find the smallest path value in the last column and trace its parents right to left
  public static void getMinPath()
  {
    path = new Node[size];
    path[0] = table[0][size-1];
    for (int i = 1; i < size; i++)
      if (table[i][size-1].pathValue() < path[0].pathValue()) path[0] = table[i][size-1];
      
    for (int i = 1; i < size; i++)
      path[i] = table[path[i-1].parent().i()][path[i-1].parent.j()];
  }
}

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

/*
  Algorithms: PSet 5 - Problem 3
  Shyam Visweswaran
*/
public class Id
{
  int i; // x coordinate
  int j; // y coordinate
  
  public Id(int i, int j)
  {
    this.i = i;
    this.j = j;
  }
  
  public int i()
  {
    return i;
  }
  
  public int j()
  {
    return j;
  }
  
  public String toString()
  {
    return (i + "-" + j);
  }
}

/*
Test run

Input grid... Legend: cell value(minimum cost)
3(3) 3(4) 5(9) 4(11) 2(11) 2(13) 
1(1) 3(4) 5(9) 6(13) 2(13) 2(13) 
4(4) 3(4) 3(7) 5(12) 4(13) 4(17) 
5(5) 6(10) 4(8) 4(9) 6(15) 4(17) 
6(6) 5(8) 1(5) 6(11) 4(13) 6(19) 
3(3) 1(4) 3(7) 4(9) 4(13) 1(12) 

Minimum cost path... Legend: coordinates[cell value]
Start -> 5-0[3] -> 5-1[1] -> 4-2[1] -> 5-3[4] -> 0-4[2] -> 5-5[1]
Input grid... Legend: cell value(minimum cost)
5(5) 5(10) 5(11) 3(11) 7(18) 5(18) 8(23) 7(25) 
6(6) 2(7) 7(14) 2(13) 2(13) 4(17) 1(18) 8(26) 
6(6) 5(11) 6(13) 7(16) 6(19) 7(20) 4(21) 6(24) 
7(7) 7(13) 5(9) 4(13) 6(16) 7(17) 8(25) 3(23) 
7(7) 3(4) 7(11) 5(10) 1(10) 7(17) 8(20) 5(19) 
1(1) 6(7) 1(5) 4(9) 5(11) 2(12) 2(14) 3(17) 
4(4) 5(6) 7(13) 1(6) 8(14) 8(19) 4(16) 4(18) 
5(5) 2(6) 2(8) 3(11) 7(13) 2(15) 8(23) 6(22) 

Minimum cost path... Legend: coordinates[cell value]
Start -> 5-0[1] -> 4-1[3] -> 5-2[1] -> 5-3[4] -> 4-4[1] -> 5-5[2] -> 5-6[2] -> 5-7[3]

*/
