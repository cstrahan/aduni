public class TestFunc2
{
  public static void main(String[] args)
  {
    int[] myInt = new int[] {-4, 0, 1};
    Poly test = new Poly(myInt);
    System.out.println(test);

    System.out.println(test.evaluate(3));
    System.out.println(test.defIntegral(0, 2, 10000000));

    
    SinFunc mySin = new SinFunc();
  }
}
