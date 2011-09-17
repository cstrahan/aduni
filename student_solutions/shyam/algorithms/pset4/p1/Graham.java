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

