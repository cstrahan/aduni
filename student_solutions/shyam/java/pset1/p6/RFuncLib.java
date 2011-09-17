/**
   FuncLib - Library file that evaluates  bracketRoot and defIntegral
   @author Shyam Visweswaran
*/
public abstract class RFuncLib
{
  /**
     Given 2 boundary values this will return the value of the root
     of the function that lies inbetween them.
     @param a boundary value
     @param b boundary value
     @param maxErr error limit
  */
  public static double bracketRoot(double a, double b, double maxErr, Function func)
  {      
    double x = (a + b) / 2;
    double f_a = func.evaluate(a);
    double f_b = func.evaluate(b);
    double f_x = func.evaluate(x);

    // if f_a and f_b have same sign return error message
    if ((f_a < 0 && f_b < 0) || (f_a > 0 && f_b > 0)) 
    {
      System.out.println("Both f(a) and f(b) have the same sign; hence cannot compute root");
      return 0;
    }
    
    // recursively compute root
    if (Math.abs(f_x) < maxErr) return (x);
    if ((f_a * f_x) >= 0)
    {
      return bracketRoot(x, b, maxErr, func);
    }
    else
    {
      return bracketRoot(a, x, maxErr, func);
    }
  }
  
  /**
     Computes the integral between 2 boundary values using the trapezoid method.
     @param a boundary value
     @param b boundary value
     @param N number of trapezoids
  */
  public static double defIntegral(double a, double b, int N, Function func)
  {
    double h = (b - a) / N;
    double sum = 0;
    for (int i = 1; i <= (N - 1); i++)
    {
      double s = a + (h * i);
      double f_s = func.evaluate(s);
      sum = sum + (h * f_s);
    }
    double f_a = func.evaluate(a);
    double f_b = func.evaluate(b);
    double result = (((f_a + f_b) / 2) * h) + sum;
    return (result);
  }
}
