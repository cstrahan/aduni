


class FuncTest3{

    static class SinFunc implements Function{
	public double evaluate(double x){
	    return(Math.sin(x));
	}
    }

    static class ExpFunc implements Function{
	public double evaluate(double x){
	    return(Math.exp(x) - 2.0);
	}
    }

    static class LogFunc implements Function{
	public double evaluate(double x){
	    return(Math.log(x) - 1);
	}
    }

    public static void main(String[] args){
	Function sin = new SinFunc();
	Function exp = new ExpFunc();
	Function log = new LogFunc();
	double x;
	x = RFuncLib.bracketRoot(1.0,4.0,0.0000000001,sin);
	System.out.println((float)x);

	x = RFuncLib.bracketRoot(0.0,4.0,0.0000000001,exp);
	System.out.println((float)x);

	x = RFuncLib.bracketRoot(1.0,4.0,0.0000000001,log);
	System.out.println((float)x);

	x = RFuncLib.defIntegral(0.0,Math.PI/2,1024*1024,sin);
	System.out.println((float)x);

	x = RFuncLib.defIntegral(0.0,2.0,1024*1024,exp);
	System.out.println((float)x);

	x = RFuncLib.defIntegral(1.0,2.0,1024*1024,log);
	System.out.println((float)x);

	testPoly();
    }

    public static void testPoly(){
	int[] coefs1 = {-3,1};
	Function p1 = new Poly(coefs1);
	double x = RFuncLib.bracketRoot(0.0,4.0,0.0000000001,p1);
	System.out.println((float)x);
	System.out.println((int)(x*x));

	int[] coefs2 = {-3,0,1};
	p1 = new Poly(coefs2);
	x = RFuncLib.bracketRoot(0.0,4.0,0.0000000001,p1);
	System.out.println((float)x);
	System.out.println((float)(x*x));

	int[] coefs3 = {-5,0,1};
	p1 = new Poly(coefs3);
	x = RFuncLib.bracketRoot(0.0,4.0,0.0000000001,p1);
	System.out.println((float)x);
	System.out.println((float)(x*x));

	int[] coefs3a = {-10,17,-8,1};
	p1 = new Poly(coefs3a);
	x = RFuncLib.bracketRoot(4.0,8.0,0.0000000001,p1);
	System.out.println((float)x);

	int[] coefs4 = {-1,-1,1};
	p1 = new Poly(coefs4);
	x = RFuncLib.bracketRoot(0.0,4.0,0.0000000001,p1);
	System.out.println((float)x);
	System.out.println((float)(1/x));
	System.out.println((float)(x - 1));


	p1 = new Poly(new int[] {4,0,-1});
	x = RFuncLib.defIntegral(0.0,2.0,1024*1024,p1);
	System.out.println((float)x);
	x = ((Poly)p1).defIntegral(0.0,2.0,1024*1024);
	System.out.println((float)x);

    }

}







