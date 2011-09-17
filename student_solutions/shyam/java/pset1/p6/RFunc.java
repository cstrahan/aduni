public abstract class RFunc
{
  public abstract double evaluate(double x);
  
  public double bracketRoot(double a, double b, double maxErr)
  {      
    double x = (a + b) / 2;
    double f_a = evaluate(a);
    double f_b = evaluate(b);
    double f_x = evaluate(x);
    if (Math.abs(f_x) < maxErr) return (x);
    if ((f_a * f_x) >= 0)
    {
      return bracketRoot(x, b, maxErr);
    }
    else
    {
      return bracketRoot(a, x, maxErr);
    }
  }
  
  public double defIntegral(double a, double b, int N)
  {
    double h = (b - a) / N;
    double sum = 0;
    for (int i = 1; i <= (N - 1); i++)
    {
      double s = a + (h * i);
      double f_s = evaluate(s);
      sum = sum + (h * f_s);
    }
    double f_a = evaluate(a);
    double f_b = evaluate(b);
    double result = (((f_a + f_b) / 2) * h) + sum;
    return (result);
  }
}
