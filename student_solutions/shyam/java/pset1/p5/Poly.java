/**
 PS-1 Problem 5 - A Polynomial Class
 Adds, subtracts, multiplies integer coefficient polynomials.
 Also finds roots and integrals of polynomials
 @author Shyam Visweswaran
 */
public class Poly extends RFunc
{
  // variables
  private int[] coef; // array for coefficients

  // constructors
  /**
     Polynomial constructor interprets c[n] as the coefficient of x^n
     @param coef expects an integer array of coefficients
  */
  public Poly(int[] coef)
  {      
    this.coef = new int[coef.length];
    System.arraycopy(coef, 0, this.coef, 0, coef.length);
  }
  
  // methods
  /**
     Polynomial degree returns the index of the highest non-zero term.
  */
  public int degree()
  {
    for (int i = coef.length - 1; i >= 0; i--)
    {
      if (coef[i] != 0) return(i);
    }
    // shouldn't get here
    return 0;
  }
    
  /**
     This displays the polynomial from the highest non-zero term to the lowest.
     Ignores terms with zero coefficients and displays appropriate signs in between terms.
  */
  public String toString()
  {
    String polyStr = "";
    for (int i = degree(); i >= 0; i--)
    {
      if (coef[i] != 0) // do stuff below only if coef is non-zero
      {
        if (coef[i] < 0 && i == degree()) // takes care of the sign of the highest term
        {
          polyStr += "-";
        }
        else if (coef[i] < 0 && i != degree())
        {
          polyStr += " - ";
        }
        else if (coef[i] > 0 && i != degree())
        {
          polyStr += " + ";
        }
        
        if (i != 0 && Math.abs(coef[i]) != 1) // takes care of other terms
        {
          polyStr += Math.abs(coef[i]) + "x";
        }
        else if (i != 0 && Math.abs(coef[i]) == 1)
        {
          polyStr += "x";
        }
        else
        {
          polyStr += Math.abs(coef[i]);
        }

        if (i > 1) polyStr += "^" + i; // add dummy variable x
      }
    }
    if (polyStr.equals("")) polyStr = "0"; // if poly is zero show 0
    polyStr = this.getClass().getName() + "[" + polyStr + "]";
    return (polyStr);
  }

  /**
   Adds two polynomials and returns a new polynomial. Checks for the length of the 2
   incoming polynomials and uses the length of the larger polynomial for the result.
   Does a term by term adding of coefficients and inserts a zero if the the shorter
   polynomial runs out of terms.
   @param a first polynomial
   @param b second polynomial
   @return polynomial
   */
  static Poly add(Poly a, Poly b)
  {
    int resultDegree = Math.max(a.degree(), b.degree());
    int[] result = new int[resultDegree + 1];
    int addenda = 0;
    int addendb = 0;
    
    for (int i = 0; i <= resultDegree; i++)
    {
      if (i <= a.degree())
      {
       addenda = a.coef[i];
      }
      else
      {
        addenda = 0;
      }
      if (i <= b.degree())
      {
        addendb = b.coef[i];
      }
      else
      {
        addendb = 0;
      }
      result[i] = addenda + addendb;
    }
    return (new Poly(result));
  }

  /**
     Multiplies 2 polynomials by using 2 loops to obtain all possible combinations
     of terms.
     @param a first polynomial
     @param b second polynomial
     @return polynomial
  */
  static Poly mul(Poly a, Poly b)
  {
    int[] result = new int[a.degree() + b.degree() + 1];
    for (int i = 0; i <= a.degree(); i++)
    {
      for (int j = 0; j <= b.degree(); j++)
      {
        result[i+j] += a.coef[i] * b.coef[j];
      }
    }
    return (new Poly(result));
  }
  
  /**
     Polynomial addition with a single argument. Calls the double argument method.
  */
  public Poly add(Poly a)
  {
    return add(this, a);
  }
  
  /**
     Polynomial multiplication with a single argument. Calls the double argument method.
  */
  public Poly mul(Poly a)
  {
    return mul(this, a);
  }

  /**
     Scales a polynomial by an integer.
     @param s scale factor
     @return polynomial with scaled coefficients
  */
  public Poly scale(int s)
  {
    int[] result = new int[this.degree() + 1];
    for (int i = 0; i <= this.degree(); i++)
    {
      result[i] += this.coef[i] * s;
    }
    return (new Poly(result));
  }
  
  /**
     Subtracts the second polynomial from the first
     @param first polynomial
     @param second polynomial
  */
  static Poly sub(Poly a, Poly b)
  {
    return a.add(b.scale(-1));
  }

  /**
     Polynomial subtraction method with a single argument. Calls the double argument method.
  */
  public Poly sub(Poly a)
  {
    return sub(this, a);
  }

  /**
     Returns the value of the function at x.
  */
  public double evaluate(double x)
  {
    double polyValue = 0.0;
    for (int i = this.degree(); i >= 0; i--)
    {
      polyValue = (polyValue * x) + this.coef[i];
    }
    return (polyValue);
  }
 
  /**
     Does polynomial integraton by symbolic integration.
  */
  public double defIntegral(double a, double b, int N)
  {
    double asum = 0.0; // running sum of terms using x = a
    double bsum = 0.0; // running sum of terms using x = b
    double avar = a; // store already calculated power of x = a
    double bvar = b; // store already calculated power of x = b
    for (int i = 0; i <= this.degree(); i++)
    {
      asum += (this.coef[i] * avar) / (i + 1);
      bsum += (this.coef[i] * bvar) / (i + 1);
      avar = avar * a;
      bvar = bvar * b;
    }
    return (bsum - asum);
  }
}







      



    

  



  
    
    


  
  

    
