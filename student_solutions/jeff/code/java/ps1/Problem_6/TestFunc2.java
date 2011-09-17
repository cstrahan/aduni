public class TestFunc2
{
  public static void main(String[] args)
  {
    int[] myInt = new int[] {-4, 0, 1};
    Poly test = new Poly(myInt);
    double MAX_ERR = .00000001;
    int ITERATIONS = 10000000;
    
    SinFunc sinFunc = new SinFunc();
    CosFunc cosFunc = new CosFunc();
    
    System.out.println(RFuncLib.bracketRoot(0, 2, MAX_ERR, cosFunc));
    System.out.println(RFuncLib.bracketRoot(3, 4, MAX_ERR, sinFunc));
    System.out.println(Poly.defIntegral(0, 2, ITERATIONS, test));
    
  }
}
