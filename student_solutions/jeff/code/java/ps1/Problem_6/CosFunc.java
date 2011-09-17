/**
   Overrides RFunc's evaluate with sin(x)
*/
class CosFunc implements Function
{
  /**
     @param x number to be evaluated
  */
  public double evaluate(double x)
  {
    System.out.println("Evaluate CosFunc called");
    return Math.cos(x);
  }
}
