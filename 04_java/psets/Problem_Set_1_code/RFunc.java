
public abstract class RFunc{

    public abstract double evaluate(double x);

    public double bracketRoot(double a, double b, double maxErr){
	double fl = evaluate(a);
	double fu = evaluate(b);
	double upper = b;
	double lower = a;
	double x = 0.0;
	double fx;
	//System.out.println("delta = " + (upper - lower));
	while((upper-lower) > maxErr){
	    x = (upper+lower)/2;
	    //System.out.println("x = " + (x));
	    fx = evaluate(x);
	    if(fx == 0) return(x);
	    else if((fx>0) && (fu>0)){ upper = x; fu = fx;}
	    else if((fx<0) && (fu<0)){ upper = x; fu = fx;}
	    else { lower = x; fl = fx;}
	}
	return(x);
    }
    
    public double defIntegral(double a, double b, int nsteps){
	int i;
	double h = (b-a)/nsteps;
	double x = a;
	double sum = (evaluate(a)+evaluate(b))/2;
	for(i=1;i<nsteps;i++){
	    x += h;
	    sum += evaluate(x);
	}
	return(sum * h);
    }

}
