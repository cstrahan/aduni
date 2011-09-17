/**
   Overrides RFunc's evaluate with cos(x)
*/
class SinFunc implements Function
{
  public double evaluate(double x)
  {
    System.out.println("Eval SinFunc called");
    return Math.sin(x);
  }
}

