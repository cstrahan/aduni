/*
   Algorithms: Pset2 - Problem 4
   Shyam Visweswaran
   Topological sorting of a directed graph by repeatedly finding a vertex of
   0 indegree and deleting it from the graph. Complexity: theta(n^2).
   Takes an array representing associations as input. Outputs a list of
   vertexes int topological order. The input is converted into a graph that
   is an array of VertexLists. Each VertexList has a Vertex id that is the
   beginning vertex and a LinkedList of Vertices. Each Vertex has 2 attributes:
   an id and indegree. A Queue (implemented as a LinkedList) is used for queuing
   VertexLists that have to be scanned.
*/
import java.util.*;

public class TopoSort
{
  static int[][] input = new int[][]
  {
    {0,1,6,7},
    {1,2,4},
    {2,3},
    {3},
    {4,3},
    {5,4},
    {6,5,7},
    {7,4}
  };

  static Vertex[] v;  // array containing Vertex objects
  static VertexList[] g; // array of VertexLists represents the graph g
  static Queue q; // queue takes VertexLists
 
  public static void main(String[] args)
  {
    makeGraph();
    sortGraph();
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
  
  public static void sortGraph()
  {
    q = new Queue(); // initialize queue
    // check the VertexLists and queue it if indegree of id-vertex is 0
    for (int i = 0; i < g.length; i++)
    {
      VertexList vl = g[i];
      if (vl.idVertex().inDegree() == 0)
        q.enqueue(vl);
    }

    // loop thru the queue and decrement indegrees of id-vertexes
    // if an id-vertex gets to indegree 0, add its VertexList to the queue
    System.out.print("Topological lists of vertices:");
    while(!q.empty())    
    {
      VertexList vl = q.dequeue();
      for (int i = 0; i < vl.size(); i++)
      {
        Vertex vertex = vl.get(i);
        vertex.decInDegree();
        if (vertex.inDegree() == 0) q.enqueue(g[vertex.id()]);
      }
      System.out.print("    " + vl.idVertex().id()); // output vertex when its VertexList comes off the queue
    }
    System.out.println();
  }
}

/*
  A vertex is an object with an id number and indegree.
*/
class Vertex
{
  private int id;
  private int inDegree;
  
  public Vertex(int id, int inDegree)
  {
    this.id = id;
    this.inDegree = inDegree;
  }
    
  public int id()
  {
    return id;
  }
  
  public int inDegree()
  {
    return inDegree;
  }
  
  public void decInDegree()
  {
    inDegree = inDegree - 1;
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
  The queue holds VertexLists that have to be scanned.
*/
class Queue
{
  private LinkedList queue;
  
  public Queue()
  {
    queue = new LinkedList();
  }
  
  public void enqueue(VertexList vl)
  {
    queue.add(vl);
  }
  
  public VertexList dequeue()
  {
    return (VertexList)queue.removeFirst();
  }
  
  public int size()
  {
    return queue.size();
  }

  public boolean empty()
  {
    if (size() == 0) return true;
    else return false;
  }
}






/*
Input graph:












Topological lists of vertices:    0    1    6    2    5    7    4    3

*/
