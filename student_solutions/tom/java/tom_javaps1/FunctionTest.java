public class FunctionTest
{
public static double bracketRoot(double a, double b, double maxerr)
	{
	    double midpoint = (a+b)/2;
	    double xmid = Math.sin(midpoint);
	    // System.out.println(Math.sin(a));
	    //System.out.println(xmid);
	    //System.out.println(midpoint);
	    if ((Math.abs(Math.sin(a)-Math.sin(midpoint))) >  maxerr)
		{   
		    if (xmid * Math.sin(a) > 0)
			return bracketRoot(midpoint, b, maxerr);
		    else  return bracketRoot(midpoint, a, maxerr);
		}
	    else return midpoint;
	}

public static double defIntegral(double a, double b, int N)
    {
	double h = (b-a)/N;
	double prev = h * ((Math.sin(a) + Math.sin(b))/2);
	int i = 1;
	while (i<(N-1))
	    {
		prev = prev + (h * (Math.sin(a + (h * i))));
		i++;
	    }
	return prev;
    }
public static void main(String[] args)
	{
	    double err = 1E-8;
	    double x = 3;
	    double y = 4;
	    System.out.println(bracketRoot(x, y, err));
	    System.out.println(defIntegral(0, 3.14159, 2048));
	    System.out.println(defIntegral(0, 6.28318, 2048));
	}

}
