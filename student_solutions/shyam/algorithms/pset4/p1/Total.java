/*
  Graham.java - Convex Hull using Graham scan; uses heapsort (since I wanted
  to get some practice in it) rather than comparable
  Algorithms: PSet 4 - Problem 1
  Shyam Visweswaran
*/

public class Graham
{
  static Stack s;
  static Point[] input; // array of points
  static int pivot; // lowest and leftmost point
  static int heapSize; // size of heap
  
  public static void run(Point[] data)
  {
    input = data;
    
    pivot = Point.getPivot(data);
    Point temp = input[pivot];
    input[pivot] = input[input.length - 1];
    input[input.length - 1] = temp;
    pivot = input.length - 1;
    heapSize = input.length - 1;
    
    heapSort();

    s = new Stack();
    s.push(input[input.length - 1]);
    s.push(input[0]);
    s.push(input[1]);
    for (int i = 2; i < (input.length - 1); i++)
    {
      while ((s.below().turn(s.top(), input[i])) != -1) s.pop();
      s.push(input[i]); 
    }

    System.out.println("Graham sort...");
    System.out.print("Start");    
    for (int i = 0; i < s.size(); i++)
      System.out.print(" --> " + s.get(i).toString());
    System.out.println();
  }
  
  // sort all points except the pivot anticlockwise using heapsort to compare polar angles
  static void heapSort()
  {
    buildHeap();
    for (int i = (heapSize - 1); i > 0; i--)
    {
      Point temp = input[i];
      input[i] = input[0];
      input[0] = temp;
      heapSize = heapSize - 1;
      heapify(0);
    }
  }
  
  static void buildHeap()
  {
    for (int i = (heapSize / 2); i >= 0; i--)
      heapify(i);
  }
  
  static void heapify(int i)
  {
    int largest;
    int l = left(i);
    int r = right(i);
    
    if ((l < heapSize) && ((input[pivot].turn(input[l], input[i])) > 0)) largest = l;
    else if ((l < heapSize) && ((input[pivot].turn(input[l], input[i])) == 0) && (Point.distance(input[pivot], input[l]) > Point.distance(input[pivot], input[i]))) largest = l;
    else largest = i;
    if ((r < heapSize) && ((input[pivot].turn(input[r], input[largest])) > 0)) largest = r;
    else if ((r < heapSize) && ((input[pivot].turn(input[r], input[largest])) == 0) && (Point.distance(input[pivot], input[r]) > Point.distance(input[pivot], input[largest]))) largest = r;
    if (largest != i)
    {
      Point temp = input[i];
      input[i] = input[largest];
      input[largest] = temp;
      heapify(largest);
    }
  }
  
  // utility for heapsort
  static int parent(int i)
  {
    return (((i + 1) / 2 ) - 1);
  }
  
  // utility for heapsort
  static int left(int i)
  {
    return (((i + 1) * 2) - 1);
  }
  
  // utility for heapsort
  static int right(int i)
  {
    return ((i + 1) * 2);
  }
}
/*
  Jarvis.java - Convex Hull using Jarvis march
  Algorithms: PSet 4 - Problem 1
  Shyam Visweswarn
*/

public class Jarvis
{
  static Point[] input; // array of points
  static Point pivotPoint; // lowest anf leftmost point
  static int arraySize; // size of points array
  
  public static void run(Point[] data)
  {
    input = data;
    arraySize = data.length;
    pivotPoint = input[Point.getPivot(input)];
    
    Point p = pivotPoint;
    int count = 1;
    while (true)
    {
      p = computeMin(p);
      if (Point.equals(pivotPoint, p)) break;
      count++;
    }
    System.out.println("Running Jarvis...");
    System.out.print("Start");
    for (int i = (input.length - count); i < input.length; i++)
      System.out.print(" --> " + input[i].toString());
    System.out.println();
  }
  
  // find the rightmost point and move it to the current end of the array and return the Point
  static Point computeMin(Point p)
  {
    int smallest = 0;
    for (int i = 1; i < arraySize; i++)
    {
      if (p.turn(input[i], input[smallest]) > 0) smallest = i;
      else if ((p.turn(input[i], input[smallest]) == 0) && ((Point.distance(p, input[i])) < Point.distance(p, input[smallest]))) smallest = i;
    }
    Point temp = input[smallest];
    input[smallest] = input[arraySize - 1];
    input[arraySize - 1] = temp;
    arraySize = arraySize - 1;
    return temp;
  }
}
/*
  Point.java - class for geometric points
  Algorithms: PSet 4 - Problem 1
  Shyam Visweswaran
*/
public class Point
{
  char name; // character name
  float x; // x-coordinate
  float y; // y-coordinate
  
  public Point(char name, float x, float y)
  {
    this.name = name;
    this.x = x;
    this.y = y;
  }
  
  public String toString()
  {
    String s = (name + "(" + x + ", " + y + ")");
    return s;
  }
  
  public float x()
  {
    return x;
  }
  
  public float y()
  {
    return y;
  }
  
  public static boolean equals(Point p1, Point p2)
  {
    if ((p1.x() == p2.x()) && (p1.y() == p2.y())) return true;
    else return false;
  }
  
  // length of the line this-p
  public static float distance(Point p1, Point p2)
  { // Euclidean distance
    float x1 = p1.x();
    float x2 = p2.x();
    float y1 = p1.y();
    float y2 = p2.y();
    return (float)Math.sqrt(((x1 - x2) * (x1 - x2)) - ((y1 - y2) * (y1 - y2)));
  }
  
  // what is the orientation of the line this-a compared to line this-b
  public int turn(Point a, Point b)
  { // clockwise = 1, anticlockwise = -1, collinear = 0
    if (((a.y() - this.y()) * (b.x() - this.x())) > ((a.x() - this.x()) * (b.y() - this.y())))
      return 1;
    else if (((a.y() - this.y()) * (b.x() - this.x())) < ((a.x() - this.x()) * (b.y() - this.y())))
      return -1;
    else return 0;
  }

  public static int getPivot(Point[] input)
  {
    int min = 0;
    for (int i = 1; i < input.length; i++)
    {
      if (input[i].y() < input[min].y()) min = i;
      else if ((input[i].y() == input[min].y()) && (input[i].x() < input[min].x())) min = i;
    }
    return min;
  }
}
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
/*
  Test.java - driver file to test the 2 Convex Hull algorithms: Graham.java
  and Jarvis.java
  Algorithms: PSet 4 - Problem 1
  Shyam Visweswaran
*/
public class Test
{
  static Point[] data = new Point[] 
  { new Point('A', 0.0F, 0.0F),
    new Point('B', -5.0F, -2.0F),
    new Point('C', -2.0F, -1.0F),
    new Point('D', -6.0F, 0.0F),
    new Point('E', -3.5F, -1.0F),
    new Point('F', -4.5F, 1.5F),
    new Point('G', -2.5F, -5.0F),
    new Point('H', 1.0F, -2.5F),
    new Point('I', 2.5F, 0.5F),
    new Point('J', -2.2F, 2.2F)};
    
  
  public static void main(String[] args)
  {
    //Graham.run(data);
    System.out.println();
    Jarvis.run(data);
  }
}

/*
Test of the 2 Convex Hull algorithms

Running Graham...
Start --> G(-2.5, -5.0) --> H(1.0, -2.5) --> I(2.5, 0.5) --> J(-2.2, 2.2) -->
  F(-4.5, 1.5) --> D(-6.0, 0.0) --> B(-5.0, -2.0)

Running Jarvis...
Start --> G(-2.5, -5.0) --> H(1.0, -2.5) --> I(2.5, 0.5) --> J(-2.2, 2.2) -->
   F(-4.5, 1.5) --> D(-6.0, 0.0) --> B(-5.0, -2.0)

*/
