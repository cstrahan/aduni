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

/*
  TestLine.java - driver class for testing Line.java
  Algorithms: PSet 4 - Problem 3
  Shyam Visweswaran
*/

public class TestLine
{
  
  public static void main(String[] args)
  {
    Point a = new Point('A', 0.0F, 0.0F);
    Point b = new Point('B', 5.0F, 5.0F);
    Point c = new Point('C', 3.0F, 1.0F);
    Point d = new Point('D', -1.0F, 4.0F);
    Point e = new Point('E', 2.0F, 2.0F);
    Point f = new Point('F', 3.0F, 3.0F);
    Point g = new Point('G', 2.0F, -2.0F);
    
    Line ab = new Line(a, b);
    Line cd = new Line(c, d);
    Line gc = new Line(g, c);
    Line ce = new Line(c, e);
    Line ae = new Line(a, e);
    Line af = new Line(a, f);
    Line eb = new Line(e, b);
    Line fb = new Line(f, b);
    Line ee = new Line(e, e);
    Line cc = new Line(c, c);
    
    // 2 intersecting lines
    System.out.println("2 intersecting lines");
    System.out.print(ab.toString() + " and " + cd.toString());
    if (ab.intersect(cd)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");
    
    // 2 non-intersecting lines
    System.out.println("2 non-intersecting lines");
    System.out.print(ab.toString() + " and " + gc.toString());
    if (ab.intersect(gc)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");
    
    // boundary case: 1 line touches the other
    System.out.println("boundary case: 1 line touches the other");
    System.out.print(ab.toString() + " and " + ce.toString());
    if (ab.intersect(ce)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // boundary case: 2 lines collinear but do not meet
    System.out.println("boundary case: 2 lines collinear but do not meet");
    System.out.print(af.toString() + " and " + eb.toString());
    if (af.intersect(eb)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // boundary case: 2 lines collinear and meet
    System.out.println("boundary case: 2 lines collinear and meet");
    System.out.print(ae.toString() + " and " + fb.toString());
    if (ae.intersect(fb)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // 1 line has 0 length and does not meet the other line
    System.out.println("1 line has 0 length and does not meet the other line");
    System.out.print(ab.toString() + " and " + cc.toString());
    if (ab.intersect(cc)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // 1 line has 0 length and meets the other line
    System.out.println("1 line has 0 length and meets the other line");
    System.out.print(ab.toString() + " and " + ee.toString());
    if (ab.intersect(ee)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // 2 lines of 0 length that do not meet
    System.out.println("2 lines of 0 length that do not meet");
    System.out.print(ee.toString() + " and " + cc.toString());
    if (ee.intersect(cc)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");

    // 2 lines of 0 length that meet
    System.out.println("2 lines of 0 length that meet");
    System.out.print(ee.toString() + " and " + ee.toString());
    if (ee.intersect(ee)) System.out.println(" intersect.");
    else System.out.println(" do not intersect.");
  }
}

/*
Test for Line.java

2 intersecting lines
Line A(0.0, 0.0) -- B(5.0, 5.0) and Line C(3.0, 1.0) -- D(-1.0, 4.0) intersect.

2 non-intersecting lines
Line A(0.0, 0.0) -- B(5.0, 5.0) and Line G(2.0, -2.0) -- C(3.0, 1.0) do not intersect.

boundary case: 1 line touches the other
Line A(0.0, 0.0) -- B(5.0, 5.0) and Line C(3.0, 1.0) -- E(2.0, 2.0) intersect.

boundary case: 2 lines collinear but do not meet
Line A(0.0, 0.0) -- F(3.0, 3.0) and Line E(2.0, 2.0) -- B(5.0, 5.0) intersect.

boundary case: 2 lines collinear and meet
Line A(0.0, 0.0) -- E(2.0, 2.0) and Line F(3.0, 3.0) -- B(5.0, 5.0) do not intersect.

1 line has 0 length and does not meet the other line
Line A(0.0, 0.0) -- B(5.0, 5.0) and Line C(3.0, 1.0) -- C(3.0, 1.0) do not intersect.

1 line has 0 length and meets the other line
Line A(0.0, 0.0) -- B(5.0, 5.0) and Line E(2.0, 2.0) -- E(2.0, 2.0) intersect.

2 lines of 0 length that do not meet
Line E(2.0, 2.0) -- E(2.0, 2.0) and Line C(3.0, 1.0) -- C(3.0, 1.0) do not intersect.

2 lines of 0 length that meet
Line E(2.0, 2.0) -- E(2.0, 2.0) and Line E(2.0, 2.0) -- E(2.0, 2.0) intersect.
*/
