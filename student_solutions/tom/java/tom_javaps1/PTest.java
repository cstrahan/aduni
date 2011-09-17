public class PTest
{    
    public static void main(String[] args)
    {
	int[] p1 = {4,3,8};
	int[] p2 = {1,1,3};
	int[] p3 = {0,1,2,0,1};
	Poly d = new Poly(p1);
	Poly e = new Poly(p2);
	Poly f = new Poly(p3);
	Poly g = (Poly.add(d,e));
	Poly h = (Poly.add(e,f));
	Poly d2 = (Poly.mul(d,e));
	Poly g2 = (d.add(e));

	System.out.println(d.toString());
	System.out.println(e.toString());
	System.out.println(f.toString());
	System.out.println(g.toString());
	System.out.println(h.toString());
	System.out.println(d2.toString());
	System.out.println(d.add(e.scale(-1)));
      
    }
}
