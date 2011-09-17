/*
  Point.java - class for geometric points
  Algorithms: PSet 4 - Problem 3
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
  
  public boolean equals(Point p)
  {
    if ((this.x() == p.x()) && (this.y() == p.y())) return true;
    else return false;
  }
  
  // length of the line this-p
  public float distance(Point p)
  { // Euclidean distance
    float x1 = this.x();
    float x2 = p.x();
    float y1 = this.y();
    float y2 = p.y();
    return (float)Math.sqrt(((x1 - x2) * (x1 - x2)) - ((y1 - y2) * (y1 - y2)));
  }
  
  // what is the orientation of the line this-a compared to line this-b
  public int turn(Point a, Point b)
  { // clockwise = 1, anticlockwise = -1, colinear = 0
    if (((a.y() - this.y()) * (b.x() - this.x())) > ((a.x() - this.x()) * (b.y() - this.y())))
      return 1;
    else if (((a.y() - this.y()) * (b.x() - this.x())) < ((a.x() - this.x()) * (b.y() - this.y())))
      return -1;
    else return 0;
  }
}
