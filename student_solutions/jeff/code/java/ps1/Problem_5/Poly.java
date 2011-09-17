/**
 * Poly.java
 *
 * Created: Wed Jan  3 15:44:26 2001
 *
 * @author Jeffrey Radcliffe
 * @version 0.3
 */
public class Poly extends RFunc
{
  private int[] polynomial;

  /**
     Default constructor 
  */
  public Poly()
  {
  }

  /**
     Constructs a polynomial from an array of integers
     @param coef an array of integer coefficients
  */
  public Poly(int[] coef)
  {
    polynomial = new int[coef.length];
    
    for (int i = 0; i < coef.length ; i++)
      polynomial[i] = coef[i];
  }

  /**
     Evaluates the polynomial
     @param x The value of x needed in order to evaluate
     @return the polynomial, evaluated with x as the variable.
  */
  public double evaluate(double x)
  {
    double answer = 0;
    for(int i = 0 ; i < this.polynomial.length; i++)
    {
      //      System.out.print(this.polynomial[i] + " * " + Math.pow(x , i) + " + ");
      answer += this.polynomial[i] * Math.pow(x , i);
    }
    return answer;
  }

  /**
     Overrides RFunc's method for defIntegral
     @param a The lower bound
     @param b The upper bound
     @param N the number of parts to be integrated
     @return the polynomial, integrated from a to b.
  */
  public double defIntegral(double a, double b, int N)
  {
    double F_a = 0;
    double F_b = 0;
    for(int i = 0; i < this.polynomial.length; i++)
      F_a += (this.polynomial[i] * Math.pow(a, (i + 1))) / (i + 1);
    for(int i = 0; i < this.polynomial.length; i++)
      F_b += (this.polynomial[i] * Math.pow(b, (i + 1))) / (i + 1);

    //    System.out.println(F_b + " - " + F_a);
    
    double answer = F_b - F_a;
    return answer;
  }

  /**
     Prints a representation of the polynomial
     @return A String displaying the polynomial
  */
  public String toString()
  {
    String temp = "Polynomial: ";
    int length = (polynomial.length - 1);
    
    for (int i = 0; i < length; i++)
      temp += polynomial[i] + "x^" + i + " + ";
    temp += polynomial[length] + "x^" + length;
    return temp;
  }

  /**
     Returns the highest degree of the polynomial
  */
  public int degree()
  {
    for(int i = (polynomial.length - 1); i > 0 ; i--)
    {
      if(polynomial[i] != 0)
        return i;
    }
    return 0;
  }
      
  /**
     Adds two polynomials
     @param a a Poly object
     @param b a Poly object
     @return a Poly that is the sum of the two arguments
  */
  public static Poly add(Poly a, Poly b)
  {
    Poly master;
    Poly slave;
    
    if (a.degree() > b.degree())
    {
      // a is longer, we will add b into it.
      master = a;
      slave = b;
    }
    else
    {
      // b is longer, we will add a into it.
      master = b;
      slave = a;
    }

    System.out.println("New polynomial will be " + master.polynomial.length);
    
    // compute a new polynomial
    for (int i = 0; i < slave.polynomial.length; i++)
      master.polynomial[i] += slave.polynomial[i];

    return master;
  }

  /**
     Instance add
     @param a a Poly object
     @return a Poly which is the sum of the instance and the argument
  */
  public Poly add(Poly a)
  {
    return add(a, new Poly(this.polynomial));
  }

  /**
     Static multiplies two polynomials
     @param a a Poly object
     @param b a Poly object
     @return a Poly that is the product of the two arguments

  */
  public static Poly mul(Poly a, Poly b)
  {
    // determine the size of the new polynomial
    int[] temp = new int[a.polynomial.length + b.polynomial.length];

    // calculate the polynomail
    for(int i = 0; i < a.polynomial.length; i++)
      for(int j = 0; j < b.polynomial.length; j++)
        temp[i + j] += a.polynomial[i] * b.polynomial[j];

    // copy the results into an array of the correct size, and
    // return the result
    int[] result = new int[a.polynomial.length + b.polynomial.length - 1];
    for(int i = 0; i < result.length; i++)
      result[i] = temp[i];
    
    return new Poly(result);
  }

  /**
     Instance mul
     @param a a Poly object
     @return a Poly which is the product of the instance and the argument
  */
  public Poly mul(Poly a)
  {
    return mul(a, new Poly(this.polynomial));
  }

  /**
     Scale a polynomial by a particular factor. Adding 
     @param constant the scalar factor
     @return A new poly, scaled by the constant
  */
  public Poly scale(int constant)
  {
    int[] result = new int[this.polynomial.length];
    for(int i = 0; i < this.polynomial.length ; i++)
      result[i] = constant * this.polynomial[i];

    return new Poly(result);
  }
}


