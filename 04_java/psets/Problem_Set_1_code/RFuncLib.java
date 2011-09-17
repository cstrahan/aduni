
public class RFuncLib{


    public static double bracketRoot(double a, double b, double maxErr,Function f){
	double fl = f.evaluate(a);
	double fu = f.evaluate(b);
	double upper = b;
	double lower = a;
	double x = 0.0;
	double fx;
	//System.out.println("delta = " + (upper - lower));
	while((upper-lower) > maxErr){
	    x = (upper+lower)/2;
	    //System.out.println("x = " + (x));
	    fx = f.evaluate(x);
	    if(fx == 0) return(x);
	    else if((fx>0) && (fu>0)){ upper = x; fu = fx;}
	    else if((fx<0) && (fu<0)){ upper = x; fu = fx;}
	    else { lower = x; fl = fx;}
	}
	return(x);
    }
    
    public static  double defIntegral(double a, double b, int nsteps,Function f){
	int i;
	double h = (b-a)/nsteps;
	double x = a;
	double sum = (f.evaluate(a)+f.evaluate(b))/2;
	for(i=1;i<nsteps;i++){
	    x += h;
	    sum += f.evaluate(x);
	}
	return(sum * h);
    }

}
