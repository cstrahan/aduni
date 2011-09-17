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
