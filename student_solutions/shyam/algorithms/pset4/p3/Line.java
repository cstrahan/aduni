/*
  Line.java - class for line segments; uses Point.java
  Algorithms: PSet4 - Problem 3
  Shyam Visweswaran
*/

public class Line
{
  Point start;
  Point end;
  
  public Line(Point start, Point end)
  {
    this.start = start;
    this.end = end;
  }

  public String toString()
  {
    return ("Line " + start.toString() + " -- " + end.toString());
  }
  
  public Point start()
  {
    return this.start;
  }
  
  public Point end()
  {
    return this.end;
  }
  
  // what is the orientation of line l compared to me (line this)
  public int orientation(Line l)
  { // clockwise = 1, anticlockwise = -1, colinear = 0
    Point o = l.start();
    Point a = l.end();
    Point b = this.end();
    if (((a.y() - o.y()) * (b.x() - o.x())) > ((b.y() - o.y()) * (a.x() - o.x()))) return 1;
    else if (((a.y() - o.y()) * (b.x() - o.x())) < ((b.y() - o.y()) * (a.x() - o.x()))) return -1;
    else return 0;
  }
  
  // what is the turn when moving from me (line this) to line l
  public int turn(Line l)
  { // right turn = 1, left turn = -1, straight on = 0
    Line m = new Line(this.start, l.end);
    return this.orientation(m);
  }

  // do lines a and b intersect
  public boolean intersect(Line a, Line b)
  {
    return a.intersect(b);
  }
  
  // does me (line this) intersect with line l
  public boolean intersect(Line l)
  {
    float x1 = this.start().x();
    float y1 = this.start().y();
    float x2 = this.end().x();
    float y2 = this.end().y();
    float x3 = l.start().x();
    float y3 = l.start().y();
    float x4 = l.end().x();
    float y4 = l.end().y();

    // check if bounding boxes intersect
    if ((max(x1, x2) >= min(x3, x4)) && (max(x3, x4) >= min(x1, x2)) && (max(y1, y2) >= min(y3, y4)) && (max(y3, y4) >= min(y1, y2)))
    { // check if lines a and b are on opposite sides of me (line this)
      Line a = new Line(this.start, l.start);
      Line b = new Line(this.start, l.end);
      if (a.orientation(this) * b.orientation(this) <= 0) return true;
      else return false;
    }
    else return false;
  }
  
  // utility to get minimum of 2 floating values
  public float min(float a, float b)
  {
    if (a <= b) return a;
    else return b;
  }
  
  // utility to get maximum of 2 floating values
  public float max(float a, float b)
  {
    if (a >= b) return a;
    else return b;
  }
}

