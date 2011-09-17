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
