public class Poly extends RFunc
    /**Problem 5, Problem set 1, Java/OOP, Tom Hickerson Jan. 3rd, 2001.
       This public class describes polynomials and their functions, modified for working
       polynomials as functions.
       @author Tom Hickerson, January 2001.
    */

{
    /** The only private variable for use here is the int array of coeffs.
	All other variables are instance-specific and cannot be called
	outside of the instance.
    */
    private double[] coeffs;
    public Poly(double[] coef)
    {
    /** Poly as itself reads an array into coeffs and sets it us as a usable, but
	not changeable, polynomial.  Note that the polynomial is "backwards"; in other
	words, the lowest degree first, then x^1, x^2, etc.
    */
	coeffs = new double[coef.length];
	
	for (int i = 0; i < coef.length; i++)
	    {
		coeffs[i] = coef[i];
	    }
	
    }

    public Poly(int[] coef)
    {
    /** Poly as itself reads an array into coeffs and sets it us as a usable, but
	not changeable, polynomial.  Note that the polynomial is "backwards"; in other
	words, the lowest degree first, then x^1, x^2, etc.
    */
	coeffs = new double[coef.length];
	
	for (int i = 0; i < coef.length; i++)
	    {
		coeffs[i] = coef[i];
	    }
	
    }
    
    public double evaluate(double x)
    {/** The new implementation of evaluate().
	 @param max The maximum degree of the polynomial begin evaluated.
	 @param ans The double that retruns the answer.
     */
	int max = this.degree();
	double ans = 0.0;
	for (int i = 0; i <= max; i++)
	    {
		ans += this.coeffs[i] * (Math.pow(x,i));
	    }
	return ans;
    }
    
    public int degree()
    { /** Degree displays the length of the polynomial.
     */
	return (coeffs.length - 1);
    }
   
    public String toString()
    { /** toString prepares the polynomial for display.
     * @param res The string which holds the answer to be sent.
     */
	String res = "";
	for (int i = 0; i < coeffs.length; i++)
	    {
		res = res + coeffs[i] + "x^" + i;
		if (i < (coeffs.length - 1)) res = res + " + ";
	    }
	
	return res;
    }
   
    public double defIntegral(double a, double b, int N)
    {	/** This is the defIntegral function changed so that polynomials can be integrated int closed form.
	    @param max The degree of the polynomial.
	    @param ans The array of doubles to be returned as the F(x).
	*/
	int max = (this.degree());
	double[] ans = new double[max + 2];
	ans[0] = 0.0;
	for (int i = 0; i <= max; i++)
	    {
		ans[i+1] = (this.coeffs[i] / (i+1));
	    }
	Poly m = new Poly (ans);
	return (m.evaluate(b) - m.evaluate(a));
    }

    public static Poly add(Poly a, Poly b)
    {  /** Add functions are both static and instances; the polynomials are checked for size
	and the function continues based on the size of the functions.
	@param temp The lowest of the two degrees.
	@param temp2 The higher of the two degrees.
	@param temp3 The array to be sent back as the answer polynomial.
       */
	int temp = 0;
	int temp2 = 0;
	if (a.degree() > b.degree()) temp = b.degree();
	else temp = a.degree();

        if (a.degree() < b.degree()) temp2 = b.degree();
	else temp2 = a.degree();
	double[] temp3 = new double[temp2+1];
	

	for (int i = 0; i <= temp; i++)
	    {
		temp3[i] = a.coeffs[i] + b.coeffs[i];		
	    }
	if (temp == b.degree()) 
	    {
		for (int j = (b.degree()+1); j <= a.degree(); j++)
		    {
			temp3[j] = a.coeffs[j];
		    }
	    }
	else for (int k = (a.degree()+1); k <= b.degree(); k++)
	    {
		temp3[k] = b.coeffs[k];
	    }
	return (new Poly(temp3));
    }
  
    public Poly add(Poly b)
    {
	int temp = 0;
	int temp2 = 0;
	if (this.degree() > b.degree()) temp = b.degree();
	else temp = this.degree();

        if (this.degree() < b.degree()) temp2 = b.degree();
	else temp2 = this.degree();
	double[] temp3 = new double[temp2+1];

	for (int i = 0; i <= temp; i++)
	    {
		temp3[i] = this.coeffs[i] + b.coeffs[i];		
	    }
	if (temp == b.degree()) 
	    {
		for (int j = (b.degree()+1); j <= this.degree(); j++)
		    {
			temp3[j] = this.coeffs[j];
		    }
	    }
	else for (int k = (this.degree()+1); k <= b.degree(); k++)
	    {
		temp3[k] = b.coeffs[k];
	    }
	return (new Poly(temp3));
    }

    public static Poly mul(Poly a, Poly b)
    {  /** Mul functions are both static and instance; they take anto account both degrees added
	together in order to create a longer function.
	@param max The two degrees added together to find the highest degree for the new polynomial.
	@param ans The answer array that will become the new polynomial.
       */
	int max = 0;

	max = (a.degree() + b.degree());
	double[] ans = new double[max + 1];
	for (int x = 0; x <= max; x++)
	    {
		ans[x] = 0;
	    }
	for (int i = 0; i <= a.degree(); i++)
	    {
		for (int j = 0; j <= b.degree(); j++)
		    {
			ans[i+j] += a.coeffs[i] * b.coeffs[j];
		    }
	    }
	return (new Poly(ans));
    }
  
    public Poly mul(Poly b)
    {
	int max = 0;

	max = (this.degree() + b.degree());
	double[] ans = new double[max + 1];
	for (int x = 0; x <= max; x++)
	    {
		ans[x] = 0;
	    }
	for (int i = 0; i <= this.degree(); i++)
	    {
		for (int j = 0; j <= b.degree(); j++)
		    {
			ans[i+j] += this.coeffs[i] * b.coeffs[j];
		    }
	    }
	return (new Poly(ans));
    }
    
    public Poly scale(int s)
    { /** Scale is implemented in order to provide subtraction by using b.scale(-1),
	for example.  It is also implemented here in both static and instance form.
	@param max The degree of the polynomial, to be used in counting.
	@param ans The array to be returned as an answer.
      */
	int max = this.degree();
	double[] ans = new double[max + 1];

	for (int i = 0; i <= max; i++)
	    {
		ans[i] = this.coeffs[i] * s;
	    }
	return (new Poly(ans));
    }
   
    public static Poly scale(Poly a, int s)
    {
	int max = a.degree();
	double[] ans = new double[max + 1];

	for (int i = 0; i <= max; i++)
	    {
		ans[i] = a.coeffs[i] * s;
	    }
	return (new Poly(ans));
    }

}

/** Sample testing code can be found in PTest.java.
    



*/

