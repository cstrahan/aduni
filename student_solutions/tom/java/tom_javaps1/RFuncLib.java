public class RFuncLib
{
    public static double bracketRoot(double a, double b, double maxerr, Function x)
    /** This is the bracketRoot function, modified to accept evaluate.
     *  @param midpoint The midpoint that helps to determine the root.
     *  @param xmid The midpoint affected byte the evaluate() statement.
     */	
       {
	    double midpoint = (a+b)/2;
	    double xmid = x.evaluate(midpoint);
	    if ((Math.abs(x.evaluate(a)-x.evaluate(midpoint))) >  maxerr)
		{   
		    if (xmid * x.evaluate(a) > 0)
			return bracketRoot(midpoint, b, maxerr, x);
		    else  return bracketRoot(midpoint, a, maxerr, x);
		}
	    else return midpoint;
	}

    public static double defIntegral(double a, double b, int N, Function y)
    /** Define the integral of an equation.
     *  @param h Part of the integral equation.
     *  @param prev The other part of the integral equation, which is modified and returned as the answer.
     */

    {
	double h = (b-a)/N;
	double prev = h * ((y.evaluate(a) + y.evaluate(b))/2);
	int i = 1;
	while (i<(N-1))
	    {
		prev = prev + (h * (y.evaluate(a + (h * i))));
		i++;
	    }
	return prev;
    }

    /** Question for problem 6: The inheritance-based approach operates under a very
     *  strict hierarchy, which makes changes difficult when dealing with large systems.
     *  However, while the interface-based approach is more flexible and extensible,
     *  (classes can accept multiple inheritances, etc.) the inheritance-based
     *  approach can deal with verying degrees of abstraction.
     */ 

}
