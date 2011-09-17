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
