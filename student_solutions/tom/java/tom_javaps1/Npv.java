public class Npv
{
    public static double npv2 (int[] dt)
    {
	double total = 1;
    for (int i = 1; i < dt.length; i++)
	{
	    total = total * (dt[i]/Math.pow(1.06, dt[i]));
	}	
    return total;
    }
public static void main (String[] args)
{
    int[] dit = new int[20];
    for (int j = 1; j < 20; j++)
	{
	    dit[j] = 100000;
	}
    //int[] dit = {1,2,3,4,5,6,7,8};
    System.out.println(npv2 (dit));
}
}
