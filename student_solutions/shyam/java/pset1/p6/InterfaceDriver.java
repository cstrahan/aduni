/**
   InterfaceDriver tests RFuncLib functions
   @author Shyam Visweswaran
*/
public class InterfaceDriver
{
  public static void main(String[] args)
  {        
      Poly my3 = new Poly(new int[] {-3, 0, 1});
      System.out.println("Root of " + my3.toString() + " is " + RFuncLib.bracketRoot(0.0, 10.0, 0.00000001, my3));
      Poly my4 = new Poly(new int[] {-1, -1, 1});
      System.out.println("Root of " + my4.toString() + " is " + RFuncLib.bracketRoot(0.0, 10.0, 0.00000001, my4));
      Poly my5 = new Poly(new int[] {-4, 0, 1});
      System.out.println("defIntegral of " + my5.toString() + " is " + RFuncLib.defIntegral(0.0, 2.0, 1000000, my5));
      
      // test to see what happens when f_a and f_b have same sign
      Poly my6 = new Poly(new int[] {3, 0, 1});
      RFuncLib.bracketRoot(0.0, 10.0, 0.00000001, my6);
    
      CosFunc myCos = new CosFunc();
      SinFunc mySine = new SinFunc();
    
      System.out.println();
      System.out.println("Root of sin(x) between 3.0 and 4.0 is " + RFuncLib.bracketRoot(3.0, 4.0, 0.00000001, mySine));
      System.out.println("Integral of sin(x) between 0 and pi is " + RFuncLib.defIntegral(0.0, 3.14159, 100000, mySine));
      System.out.println("Integral of sin(x) between 0 and 2*pi is " + RFuncLib.defIntegral(0.0, (2 * 3.14159), 100000, mySine));
      System.out.println();        
      System.out.println("Root of cos(x) between 3.0 and 4.0 is " + RFuncLib.bracketRoot(1.0, 3.0, 0.00000001, myCos));        
      System.out.println("Integral of cos(x) between 0 and pi/2 is " + RFuncLib.defIntegral(0.0, (3.14159/2), 100000, myCos));
      System.out.println("Integral of cos(x) between 0 and pi is " + RFuncLib.defIntegral(0.0, 3.14159, 100000, myCos));
  }
}

