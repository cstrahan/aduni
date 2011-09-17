public class PowerCalc
{
    public static void main(String[] args)
    {
	double x = 5.0;
	System.out.println(x + " to the power 4 is " + power(x,4));
	System.out.println(7.5 + " to the power 5 is " + power(7.5,5));
	System.out.println(7.5 + " to the power 0 is " + power(7.5,0));
	System.out.println(10 + " to the power -2 is " + power (10, -2));
    }

    // Raise x to the power n
    static double power(double x, int n)
    {
	if(n > 1)
	    return x*power(x, n-1);
	else if(n < 0)
	    return 1.0/power(x, -n);
	else
	    return n == 0 ? 1.0 : x;
    }
}
	
