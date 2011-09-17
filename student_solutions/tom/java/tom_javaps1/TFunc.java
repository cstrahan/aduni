public class TFunc
{
    public static void main(String[] args)
    {
	double max = 1E-8;
	double a = 1;
	double b = 3;
	CosFunc x = new CosFunc();
	System.out.println(x.bracketRoot(a,b,max));
	double w = 0.0;
	double y = 1.570795;
	double z = 3.14159;
	int n = 1024;
	CosFunc c = new CosFunc();
	System.out.println(c.defIntegral(w,y,n));
	System.out.println(c.defIntegral(w,z,n));
    }
}
