/**
 * Poly.java
 *
 *
 * Created: Wed Jan  3 15:44:26 2001
 *
 * @author Jeffrey Radcliffe
 * @version 0.1
 */
public class Poly
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
     @param coef An array of integer coefficients
  */
  public Poly(int[] coef)
  {
    polynomial = new int[coef.length];

    for (int i = 0; i < coef.length ; i++)
      polynomial[i] = coef[i];
  }

  /**
     Returns a representation of the Polynomial
     @return String The Polynomial
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
     @return int highest degree of the Poly
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
     @return Poly that is the sum of the two arguments
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

    // compute a new polynomial
    for (int i = 0; i < slave.polynomial.length; i++)
      master.polynomial[i] += slave.polynomial[i];

    return master;
  }

  /**
     Adds the current instance of the Poly with Poly <code>a</code>
     @param a a Poly object
     @return Poly which is the sum of the instance and the argument
  */
  public Poly add(Poly a)
  {
    return add(a, new Poly(this.polynomial));
  }

  /**
     Multiplies two polynomials
     @param a a Poly object
     @param b a Poly object
     @return Poly that is the product of the two arguments
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
     Multiplies the current instance of the Poly with another Poly <code>a</code>
     @param a a Poly object
     @return a Poly which is the product of the instance and the argument
  */
  public Poly mul(Poly a)
  {
    return mul(a, new Poly(this.polynomial));
  }

  /**
     Scales a polynomial by a particular factor.
     This can be used in conjunction with method <code>add</code>
     for subtraction.
     @param constant the scalar factor
     @see #add
  */
  public Poly scale(int constant)
  {
    int[] result = new int[this.polynomial.length];
    for(int i = 0; i < this.polynomial.length ; i++)
      result[i] = constant * this.polynomial[i];

    return new Poly(result);
  }
}


