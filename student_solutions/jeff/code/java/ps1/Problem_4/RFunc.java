/**
 * RFunc.java
 *
 * A class to represent the behaviour of functions
 * over the real numbers.
 *
 * Created: Wed Jan  3 15:56:45 2001
 * @author Jeffrey Radcliffe
 * @version 0.2
 */
public abstract class RFunc 
{
  /**
     Will be overriden in child classes
     @param x The value to be evaluated
     @return f(x)
  */
  public abstract double evaluate(double x);

  /**
     A method to find the root between two numbers int a continuous function
     This is the recursive version (iterative version is within problem 6!)
     @param a
     @param b
     @param maxErr amount of error tolerance
     @return a double which is the value of x for which f(x) = 0
  */
  public double bracketRoot(double a, double b, double maxErr)
  {
    double x = (a + b) / 2.0 ;
    double f_a = evaluate(a);
    double f_b = evaluate(b);
    double f_x = evaluate(x);

    // check to see if we are close enough!
    if(Math.abs(f_x) < maxErr) return x;
    
    // Are f_a and f_x the same sign?
    if((f_a > 0 && f_x > 0 ) || f_a < 0 && f_x < 0) 
    {
      // x < R < b
      return bracketRoot(x, b, maxErr);
    }
    else
    {
      // a < R < x
      return bracketRoot(a, x, maxErr);
    }
  }

  /**
     A method to caluculate definite integrals through use
     of trapezoidal approximaton
     @param a Lower end of range
     @param b Upper end of range
     @param N The number of parts to be integrated by
     @return The definite integral of F, from a to b
  */
  public double defIntegral(double a, double b, int N)
  {
    double h = (b - a) / N;
    double integral;

    // calculate the integral
    integral = h * ((evaluate(a) + evaluate(b)) / 2.0);
    for(int i = 1 ; i < (N - 1) ; i++)
      integral += (h * evaluate(a + (h * i)));

    return integral;
  }
}
