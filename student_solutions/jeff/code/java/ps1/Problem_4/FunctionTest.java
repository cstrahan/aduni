public class FunctionTest
{
  public static void main(String[] args)
  {
    double a = 3;
    double b = 4;
    double maxErr = .00000001;

    System.out.println("a = " + a + "\n" +
                       "b = " + b + "\n" +
                       "maxErr = " + maxErr + "\n");

    double answer1 = bracketRoot(a, b, maxErr);
    System.out.println("The root is " + answer1);
    System.out.println();
    
    double answer2  = defIntegral(0, answer1, 32768);
    System.out.println("The integral of sin(x) from 0 to pi is " + answer2);

    System.out.println();
    double answer3  = defIntegral(0, (2 * answer1), 100000);
    System.out.println("The integral sin(x) from 0 to 2pi is " + answer3);

  }

  /**
     A method to find the root between two numbers int a continuous function
     in this case, f(x) = sin(x)
  */
  public static double bracketRoot(double a, double b, double maxErr)
  {
    double x = (a + b) / 2;
    double f_a = Math.sin(a);
    double f_b = Math.sin(b);
    double f_x = Math.sin(x);

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
     our function is f(x) = sin(x)
  */
  public static double defIntegral(double a, double b, int N)
  {
    double h = (b - a) / N;
    double integral;
    
    integral = h * ((Math.sin(a) + Math.sin(b)) / 2);
    for(int i = 1 ; i < (N - 1) ; i++)
        integral += (h * Math.sin(a + (h * i)));

    return integral;
  }
}

