public class Hello 
{

    public static int fact (int a)
    {
	if (a<=1) return 1;
	return (a * fact (a-1));
    }
    public static int fact2 (int a) 
    {
	int b = 0;
	int c = 1;
	while (b<a) 
	    {
	    b++;
	    c=c*b;
	    }
	return c;
    }

    public static int fib (int a) 
    {
	if (a<=1) return 1;
	return ((fib(a-1)) + (fib(a-2)));
    }
    public static int fib2 (int x) 
    {
	    int y = 1;
	    int count = 0;
	    int z = 1;
	    int w = 0;
	    while (count<x) 
		{
		w = y;
		count++;
		y = z;
		z = z + w;
		}
	    return y;
     }
     public static void main (String[] args)
     {
	    int a = 5;
	    int fa = fib2(a);
	    System.out.println(fa);
	    if (args.length==2) 
		{
		    if (args[0].equals("I")) System.out.println(fib2(Integer.parseInt(args[1])));
		    else if (args[0].equals("R")) System.out.println(fib(Integer.parseInt(args[1])));
		    else System.out.println(args[1]);
                }
     }
}






