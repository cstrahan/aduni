public class WhileLoop
{
    public static void main(String[] args)
    {
	int limit = 20;
	int sum = 0;
	int i = 1;

	// Loop from 1 to the value of limit, adding 1
	while(i <= limit)
	    sum += i++;
	System.out.println("sum = " + sum);
    }
}
