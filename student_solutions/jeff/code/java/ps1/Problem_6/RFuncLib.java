/**
 * RFuncLib.java
 *
 * A class to represent the behaviour of functions
 * over the real numbers.
 *
 * Created: Wed Jan  3 18:32:16 2001
 *
 * @author Jeffrey Radcliffe
 * @version 0.4
 */

public abstract class RFuncLib
{
  /**
     Method to find the root between two numbers int a continuous function
     This is the iterative version
     @param a
     @param b
     @param maxErr amount of error tolerance
     @return the value of x where f(x) = 0 (within tolerance)
  */
  public static double bracketRoot(double a, double b, double maxErr, Function func)
  {
    // System.out.println("RFuncLib bracketRoot called");
    // System.out.println("a = " + a + " b = " + b + " maxErr = " + maxErr + " func = " + func);    

    double x = (a + b) / 2;
    double f_a = func.evaluate(a);
    double f_b = func.evaluate(b);
    double f_x = func.evaluate(x);
    // int count = 0;
    
    for(;;)
    {

      // recalculate variables
      x = (a + b) / 2;
      f_a = func.evaluate(a);
      f_b = func.evaluate(b);
      f_x = func.evaluate(x);

      // System.out.println("a = " + a + " b = " + b + " x = " + x);      
      // System.out.println("f(a) = " + f_a + " f(b) = " + f_b + " f(x) = " + f_x);

      // check to see if values are right
      if((f_a > 0 && f_b > 0) || (f_a < 0 & f_b < 0))
      {
        System.out.println("bad bracket values");
        return 0;
      }
    
      // check to see if we are close enough!
      if(Math.abs(f_x) < maxErr)
        {
          // System.out.println("Count is "  + count);
          return x;
        }
    
      // Are f_a and f_x the same sign?
      if((f_a > 0 && f_x > 0 ) || f_a < 0 && f_x < 0) 
      {
        // x < R < b
        a = x;
      }
      else
      {
        // a < R < x
        b = x;
      }
      //      count++;
    }
  }

  /**
     A method to caluculate definite integrals through use
     of trapezoidal approximaton
  */
  public static double defIntegral(double a, double b, int N, Function func)
  {
    //    System.out.println("RFuncLib defIntegral called");


    // System.out.println("a = " + a + " b = " + b + " N = " + N + " func = " + func);

    double h = (b - a) / N;
    double integral;
    
    integral = h * ((func.evaluate(a) + func.evaluate(b)) / 2);
    for(int i = 1 ; i < (N - 1) ; i++)
      integral += (h * func.evaluate(a + (h * i)));
    return integral;
  }
}


   
  

