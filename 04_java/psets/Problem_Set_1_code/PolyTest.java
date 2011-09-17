


class PolyTest{

    public static void main(String[] args){
	int[] coefs1 = {1,1};
	int[] coefs2 = {1,1,2,4,8};
	int[] coefs3 = {1,-1,0,3,8};

	Poly p1 = new Poly(coefs1);
	Poly p2 = new Poly(coefs2);
	Poly p3 = new Poly(coefs3);
	Poly zerop = new Poly(new int[] {0});
	System.out.println(p1.degree());
	System.out.println(p1);
	System.out.println(p1.scale(5));
	Poly mp1 = p1.scale(-1);
	System.out.println(mp1);
	System.out.println(p1.add(mp1));
	System.out.println(p2);
	System.out.println(p3);
	System.out.println((p3.add(zerop)).toString());
	System.out.println((p3.add(p1)).toString());
	System.out.println(p1.add(p2));
	System.out.println(Poly.add(p1,p2));
	Poly p4 = p1.mul(p1);
	System.out.println(p4);
	Poly p5 = p4.mul(p1);
	System.out.println(p5);
	p4 = p4.mul(p4);
	System.out.println(p4);
	p4 = p4.mul(p4);
	System.out.println(p4);
	System.out.println(p4.degree());
    }
}
