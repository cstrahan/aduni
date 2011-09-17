/**
 * PolyDriver2 tests RFunc for Poly roots and integration
 * @author Shyam Visweswaran
 */
public class PolyDriver2
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
    System.out.println();
    Poly my3 = new Poly(new int[] {-3, 0, 1});
    System.out.println("Root of " + my3.toString() + " is " + my3.bracketRoot(0.0, 10.0, 0.00000001));
    Poly my4 = new Poly(new int[] {-1, -1, 1});
    System.out.println("Root of " + my4.toString() + " is " + my4.bracketRoot(0.0, 10.0, 0.00000001));
    Poly my5 = new Poly(new int[] {-4, 0, 1});
    System.out.println("defIntegral of " + my5.toString() + " is " + my5.defIntegral(0.0, 2.0, 1000000));

  }
}
