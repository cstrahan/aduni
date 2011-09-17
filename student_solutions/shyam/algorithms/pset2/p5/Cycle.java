/*
   Algorithms: Pset2 - Problem 5
   Shyam Visweswaran
   Finding a cycle in an undirected graph. Outputs the first cycle found.
*/
import java.util.*;

public class Cycle
{
  static int[][] input = new int[][]
  {
    {0,1,4},
    {1,0,2,3},
    {2,1,3},
    {3,1,2,4},
    {4,0,3},
  };

  static int NONE = -1; // vertex has no parent
  static int VISITED = 1; // vertex visited
  static int UNVISITED = 0; // vertex not yet visited
    
  static Vertex[] v;  // 2-D array of vertex objects
  static VertexList[] g; // 2-D array of association lists for each vertex
  static Stack s; // stack for storing vertices during DFS
  
  public static void main(String[] args)
  { // make a graph with input array and do DFS search
    makeGraph();
    s = new Stack();
    for (int i = 0; i < v.length; i++)
      if (v[i].status() == UNVISITED) dfsCycle(v[i]);
    System.out.println("No cycles found.");
  }
  
  public static void makeGraph()
  {
    v = new Vertex[input.length]; // initialize array of vertices
    int[] count = new int[input.length]; // array for keeping indegree count
    
    // go thru input array and get indegree count and store in count array
    for (int i = 0; i < input.length; i++)
      for (int j = 1; j < input[i].length; j++)
        count[input[i][j]]++;
    
    // create the vertices
    for (int i = 0; i < count.length; i++)
      v[i] = new Vertex(i, count[i]);
    
    g = new VertexList[input.length]; // initialize graph array
    for (int i = 0; i < input.length; i++) // create VertexLists for each association list
    {
      VertexList vl = new VertexList(v[input[i][0]]);
      for (int j = 1; j < input[i].length; j++)
        vl.add(v[input[i][j]]);
      g[i] = vl;
    }
  }
    
  public static void dfsCycle(Vertex vertex)
  {
    vertex.setStatus(VISITED);
    s.push(vertex); // save vertex on stack
    VertexList vl = g[vertex.id()];
    for (int i = 0; i < vl.size(); i++)
    { // first look for a possible 'cycle' vertex; make sure it is not a back edge
      Vertex newVertex = vl.get(i);
      if ((newVertex.status() == VISITED) && (newVertex.id() != vertex.parent()))
      {
        s.push(newVertex);
        showCycle();
      } // then recursive call dfsCycle on unvisited vertex
      else if (newVertex.status() == UNVISITED)
      {
        newVertex.setParent(vertex.id());
        dfsCycle(newVertex);
      }
    }
  }

  // pop the vertices from the stack till the vertex matches the first one popped;
  // print out and exit once cycle is complete
  public static void showCycle()
  {
    System.out.print("Cycle found: ");
    int startOfCycle = s.pop().id();
    System.out.print(startOfCycle);
    while(s.size() != 0)
    {
      Vertex vertex = s.pop();
      System.out.print(" --> " + vertex.id());
      if (vertex.id() == startOfCycle) break;
    }
    System.out.println();
    System.exit(0);
  }
}

/*
  A vertex is an object with an id number and indegree.
*/
class Vertex
{
  private int id;
  private int status;
  private int parent;
  private int start;
  private int finish;
  
  public Vertex(int id, int inDegree)
  {
    this.id = id;
    status = Cycle.UNVISITED;
    parent = Cycle.NONE;
    start = 0;
    finish = 0;
  }
    
  public int id()
  {
    return id;
  }
  
  public int status()
  {
    return status;
  }

  public void setStatus(int status)
  {
    this.status = status;
  }

  public int parent()
  {
    return parent;
  }
  
  public void setParent(int parent)
  {
    this.parent = parent;
  }
  
  public int start()
  {
    return start;
  }
  
  public void setStart(int start)
  {
    this.start = start;
  }
  
  public int finish()
  {
    return finish;
  }

  public void setFinish(int finish)
  {
    this.finish = finish;
  }
}

/*
  VertexLists are LinkedLists with an id-vertex and a lists of its neighbors.
*/
class VertexList
{
  Vertex id;
  LinkedList vertexList;
  
  public VertexList(Vertex vertex)
  {
    id = vertex;
    vertexList = new LinkedList();
  }
  
  public Vertex idVertex()
  {
    return id;
  }
  
  public void add(Vertex vertex)
  {
    vertexList.add(vertex);
  }
  
  public Vertex get(int n)
  {
    return (Vertex)vertexList.get(n);
  }
  
  public int size()
  {
    return vertexList.size();
  }
  
  public boolean empty()
  {
    if (size() == 0) return true;
    else return false;
  }
}

/*
  The stack holds vertices that have been discovered.
*/
class Stack
{
  private LinkedList stack;
  
  public Stack()
  {
    stack = new LinkedList();
  }
  
  public void push(Vertex vertex)
  {
    stack.add(vertex);
  }
  
  public Vertex pop()
  {
    return (Vertex)stack.removeLast();
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

/*
Input graph:













Result on running Cycle.java
Cycle found: 1 --> 3 --> 2 --> 1

*/
