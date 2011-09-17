
class FuncTest2{

    public static void main(String[] args){
	int[] coefs1 = {-3,1};
	RFunc p1 = new Poly(coefs1);
	double x = p1.bracketRoot(0.0,4.0,0.0000000001);
	System.out.println((float)x);

	int[] coefs2 = {-3,0,1};
	p1 = new Poly(coefs2);
	x = p1.bracketRoot(0.0,4.0,0.0000000001);
	System.out.println((float)x);
	System.out.println((float)(x*x));

	int[] coefs3 = {-5,0,1};
	p1 = new Poly(coefs3);
	x = p1.bracketRoot(0.0,4.0,0.0000000001);
	System.out.println((float)x);

	int[] coefs3a = {-10,17,-8,1};
	p1 = new Poly(coefs3a);
	x = p1.bracketRoot(4.0,8.0,0.0000000001);
	System.out.println((float)x);

	int[] coefs4 = {-1,-1,1};
	p1 = new Poly(coefs4);
	x = p1.bracketRoot(0.0,4.0,0.0000000001);
	System.out.println((float)x);
	System.out.println((float)(1/x));
	System.out.println((float)(x - 1));


	p1 = new Poly(new int[] {4,0,-1});
	x = p1.defIntegral(0.0,2.0,1024*1024);
	System.out.println((float)x);

    }
}

