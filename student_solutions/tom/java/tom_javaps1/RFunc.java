abstract class RFunc
{

    public abstract double evaluate(double x);
 
    public double bracketRoot(double a, double b, double maxerr)
    /** This is the bracketRoot function, modified to accept evaluate.
     *  @param midpoint The midpoint that helps to determine the root.
     *  @param xmid The midpoint affected byte the evaluate() statement.
     */	
       {
	    double midpoint = (a+b)/2;
	    double xmid = evaluate(midpoint);
	    if ((Math.abs(evaluate(a)-evaluate(midpoint))) >  maxerr)
		{   
		    if (xmid * evaluate(a) > 0)
			return bracketRoot(midpoint, b, maxerr);
		    else  return bracketRoot(midpoint, a, maxerr);
		}
	    else return midpoint;
	}

    public double defIntegral(double a, double b, int N)
    /** Define the integral of an equation.
     *  @param h Part of the integral equation.
     *  @param prev The other part of the integral equation, which is modified and returned as the answer.
     */

    {
	double h = (b-a)/N;
	double prev = h * ((evaluate(a) + evaluate(b))/2);
	int i = 1;
	while (i<(N-1))
	    {
		prev = prev + (h * (evaluate(a + (h * i))));
		i++;
	    }
	return prev;
    }

}
 
