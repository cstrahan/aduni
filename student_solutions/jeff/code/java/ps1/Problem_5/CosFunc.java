/**
   Overrides RFunc's evaluate with sin(x)
*/
public class CosFunc extends RFunc
{
  /**
     @param x number to be evaluated
  */
  public double evaluate(double x)
  {
    return Math.cos(x);
  }
}
