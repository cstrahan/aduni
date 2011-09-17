public class PTest2 
{
	public static void main(String[] args)
	{
       	double[] p1 = {-3,0,1};
	double[] p2 = {-1,-1,1};
	double[] p3 = {-4,0,1};
	Poly d = new Poly(p1);
	Poly e = new Poly(p2);
	Poly f = new Poly(p3);

	double aa = 0.0;
	double bb = 2.0;
	int cc = 1024;

	double a2 = -2.0;
	double b2 = 2.0;

	double a3 = -1.0;
	double b3 = 1.0;
	double max = 1E-8;

	//double g2 = (d.evaluate(1.0));
	double g2 = (d.bracketRoot(a2,b2,max));
	double g = (f.defIntegral(aa,bb,cc,Poly));
	double h = (e.bracketRoot(a3,b3,max));

	System.out.println(g2);
	System.out.println(g);
	System.out.println(h);

	}

}
