public class FunctionTest
{
  public static void main(String[] args)
  {
    System.out.println("Root is " + bracketRoot(3.0, 4.0, 0.00000001));
    System.out.println("Integral of sin(x)-pi is " + defIntegral(0.0, 3.14159, 100000));
    System.out.println("Integral of sin(x)-two-pi is " + defIntegral(0.0, (2 * 3.14159), 100000));
  }
    
  public static double bracketRoot(double a, double b, double maxErr)
  {      
    double x = (a + b) / 2;
    double f_a = Math.sin(a);
    double f_b = Math.sin(b);
    double f_x = Math.sin(x);
    if (Math.abs(f_x) < maxErr) return(x);
    if ((f_a * f_x) >= 0)
    {
      return bracketRoot(x, b, maxErr);
    }
    else
    {
      return bracketRoot(a, x, maxErr);
    }
  }
  
  public static double defIntegral(double a, double b, int N)
  {
    double h = (b - a) / N;
    double sum = 0;
    for (int i = 1; i <= (N - 1); i++)
    {
      double s = a + (h * i);
      double f_s = Math.sin(s);
      sum = sum + (h * f_s);
    }
    double f_a = Math.sin(a);
    double f_b = Math.sin(b);
    double result = (((f_a + f_b) / 2) * h) + sum;
    return(result);
  }
}

  
    




