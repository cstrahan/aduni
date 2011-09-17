
class FuncTest{

    static class SinFunc extends RFunc{
	public double evaluate(double x){
	    return(Math.sin(x));
	}
    }

    static class ExpFunc extends RFunc{
	public double evaluate(double x){
	    return(Math.exp(x) - 2.0);
	}
    }

    static class LogFunc extends RFunc{
	public double evaluate(double x){
	    return(Math.log(x) - 1);
	}
    }


    public static void main(String[] args){
	RFunc sin = new SinFunc();
	RFunc exp = new ExpFunc();
	RFunc log = new LogFunc();
	double x;
	x = sin.bracketRoot(1.0,4.0,0.0000000001);
	System.out.println((float)x);
	System.out.println((float)Math.PI);

	x = exp.bracketRoot(0.0,4.0,0.0000000001);
	System.out.println((float)x);
	System.out.println((float)Math.log(2));

	x = log.bracketRoot(1.0,4.0,0.0000000001);
	System.out.println((float)x);
	System.out.println((float)Math.E);

	x = sin.defIntegral(0.0,Math.PI/2,1024*1024);
	System.out.println((float)x);
	System.out.println((float)1.0);

	x = exp.defIntegral(0.0,2.0,1024*1024);
	System.out.println((float)x);
	System.out.println((float)(Math.exp(2) - 5));

	x = log.defIntegral(1.0,2.0,1024*1024);
	System.out.println((float)x);
	System.out.println((float)(2*Math.log(2) - 2));

    }
}

