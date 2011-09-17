


/**
 * Implements <b>immutable</b> polynomials with integer coefficients
 */
public class Poly extends RFunc implements Function{
    private int coefs[] = {0};

     /**
      * Constructor from coefficient array
      */
    public Poly(int[] coefs){
	if(coefs != null){
	    this.coefs = new int[coefs.length];
	    for(int i=0;i<coefs.length;i++){
		this.coefs[i] = coefs[i];
	    }
	}
    }

     /**
      * Return a new polynomial with coefficients scaled by s.
      */
    public Poly scale(int s){
	Poly p = new Poly(coefs);
	for(int i=0;i<coefs.length;i++){
	    p.coefs[i] = s * coefs[i];
	}
	return(p);
    }

    private double evaluate(double[] coefs, double x){
	double val = 0;
	for(int i=coefs.length-1;i>=0;i--){
	    val = (val * x) + coefs[i];
	}
	return(val);
    }
     /**
      * Return the definate integral using anti-derivative.
      */
    public double defIntegral(double a, double b, int N){
	double[] newcoefs = new double[coefs.length + 1];
	newcoefs[0] = 0;
	for(int i=0;i<coefs.length;i++){
	    newcoefs[i+1] = coefs[i]/((double)(i+1));
	}
	return(evaluate(newcoefs,b) - evaluate(newcoefs,a));

    }

     /**
      * Return the degree of the polynomial.
      */
    public int degree(){
	int i;
	int degree = 0;;
	for(i=0;i<coefs.length;i++){
	    if(coefs[i] != 0) degree = i;
	}
	return(degree);
    }

     /**
      * Return a string representation of the polynomial.
      */
    public String toString(){
	int i;
	String buf = "";
	for(i=coefs.length-1;i>=0;i--){
	    if(coefs[i] == 0){
		if(degree() > 0 || i > 0) continue;
	    }
	    if(i > 0){
		buf += coefs[i] + " x^" + i;
		buf += " + ";
	    }
	    else
		buf += coefs[i];
	}
	return(buf);
    }

     /**
      * Evaluate the polynomial at x.
      */
    public double evaluate(double x){
	int i;
	double val = 0;
	for(i=coefs.length-1;i>=0;i--){
	    val = (val * x) + coefs[i];
	}
	return(val);
    }

     /**
      * Returns the sum of two polynomials.
      */
    public Poly add(Poly a){
	int len = coefs.length;
	if(a.coefs.length > len)
	    len = a.coefs.length;
	int[] newcoefs = new int[len];
	for(int i=0;i<len;i++){
	    newcoefs[i] = 0;	    
	    if(i<coefs.length)
		newcoefs[i] += coefs[i];
	    if(i<a.coefs.length)
		newcoefs[i] += a.coefs[i];
	}
	return(new Poly(newcoefs));
    }

     /**
      * Static routine to sum two polynomials.
      */
    public static Poly add(Poly a, Poly b){
	int len = b.coefs.length;
	if(a.coefs.length > len)
	    len = a.coefs.length;
	int[] newcoefs = new int[len];
	for(int i=0;i<len;i++){
	    newcoefs[i] = 0;	    
	    if(i<b.coefs.length)
		newcoefs[i] += b.coefs[i];
	    if(i<a.coefs.length)
		newcoefs[i] += a.coefs[i];
	}
	return(new Poly(newcoefs));
    }

     /**
      * Returns the product of two polynomials.
      */
    public Poly mul(Poly a){
	int i;
	int len = coefs.length;
	int lena = a.coefs.length;
	int[] newcoefs = new int[len + lena - 1];
	for(i=0;i<len+lena-1;i++) newcoefs[i] = 0;
	for(i=0;i<len;i++){
	    for(int j=0;j<lena;j++){
		newcoefs[i+j] += coefs[i] * a.coefs[j];
	    }
	}
	return(new Poly(newcoefs));
    }

}


