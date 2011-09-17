/**
 * PolyDriver is a test file for Poly.java
 * @author Shyam Visweswaran
 */
public class PolyDriver
{
  public static void main(String[] args)
  {
    Poly my1 = new Poly(new int[] {1, -4, 5, 6});
    Poly my2 = new Poly(new int[] {21, 54, 32, 0, -8, 0, -2});
    System.out.println("my1 is " + my1.toString());
    System.out.println("my2 is " + my2.toString());
    System.out.println("Adding my1 and my2 gives " + Poly.add(my1, my2).toString());
    System.out.println("Multiplying my1 and my2 gives  " + Poly.mul(my1, my2).toString());
    System.out.println("Subtracting my2 and my2 gives " + Poly.sub(my2, my2).toString());
  }
}
