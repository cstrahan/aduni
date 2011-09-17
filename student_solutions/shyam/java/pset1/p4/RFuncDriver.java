public class RFuncDriver
{
  public static void main(String[] args)
  {
    SinFunc mySine = new SinFunc();
    CosFunc myCos = new CosFunc();
    
    System.out.println("Root of sin(x) between 3.0 and 4.0 is " + mySine.bracketRoot(3.0, 4.0, 0.00000001));
    System.out.println("Integral of sin(x) between 0 and pi is " + mySine.defIntegral(0.0, 3.14159, 100000));
    System.out.println("Integral of sin(x) between 0 and 2*pi is " + mySine.defIntegral(0.0, (2 * 3.14159), 100000));
    System.out.println();        
    System.out.println("Root of cos(x) between 3.0 and 4.0 is " + myCos.bracketRoot(1.0, 3.0, 0.00000001));        
    System.out.println("Integral of cos(x) between 0 and pi/2 is " + myCos.defIntegral(0.0, (3.14159/2), 100000));
    System.out.println("Integral of cos(x) between 0 and pi is " + myCos.defIntegral(0.0, 3.14159, 100000));
  }
}
