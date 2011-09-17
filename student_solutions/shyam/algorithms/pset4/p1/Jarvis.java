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

 
