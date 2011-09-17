public class PQtest
{

    public static void main(String[] args)
    {
	//String x = "abc";
	//String y = "mno";
	//String z = "xyz";
	
	Integer z1 = new Integer(21);
	Integer z2 = new Integer(22);
	
	String z3 = "right";

	PQueue test1 = new PQueue();
	//test1.insert(x);
	//System.out.println(test1.length());
	//test1.insert(z);
	//test1.insert(y);
	test1.insert(z3);
	System.out.println(test1.length());
	test1.insert(z2);
	//System.out.println(test1.removeMax());
	System.out.println(test1.length());
	System.out.println(test1.removeMax());
	test1.insert(z1);
	System.out.println(test1.length());
	System.out.println(test1.removeMax());
	    
    }
}
