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
