public class RTest3
{
    public static void main(String[] args)
    {
      	double[] p1 = {-3,0,1};
	double[] p2 = {-1,-1,1};
	double[] p3 = {-4,0,1};
	Poly d = new Poly(p1);
	Poly e = new Poly(p2);
	Poly f = new Poly(p3);
	CosFunc x1 = new CosFunc();
	double g = (RFuncLib.defIntegral(0.0,2.0,1024,f));
	double xx = (RFuncLib.bracketRoot(1.0,3.0,1E-8,x1));
	System.out.println(xx);
    }
}
