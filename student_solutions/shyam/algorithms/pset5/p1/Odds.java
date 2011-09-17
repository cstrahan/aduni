/*
  Odds.java
  Algorithms: PSet 5 - Problem 1
  Shyam Visweswaran
*/
public class Odds
{
  private static final double NULL = -1000; // last cell int the table has no value
  private static double[][] table; // for computing probabilities
  
  public static void main(String[] args)
  {
    if (args.length == 1)
      doOdds(Integer.parseInt(args[0]));
    else doOdds(7); // default input
    displayOdds(table);
  }
  
  // compute the probabilities right to left, down to up
  public static void doOdds(int k)
  {
    int n = k/2 + 1;
    table = new double[n + 1][n + 1];
    for (int i = n; i >= 0; i--)
    {
      for (int j = n; j >= 0; j--)
      {
        if ((i == n) && (j == n)) table[i][j] = NULL;
        else if (j == n) table[i][j] = 0.0;
        else if (i == n) table[i][j] = 1.0;
        else table[i][j] = ((table[i+1][j] + table[i][j+1]) / 2);
      }
    }
  }
 
  // utility for displaying table 
  public static void displayOdds(double[][] table)
  {
    for (int i = 0; i < table.length; i++)
    {
      for (int j = 0; j < table[i].length; j++)
      {
        if (table[i][j] == NULL) System.out.println("x");
        else System.out.print(table[i][j] + " ");
      }
      System.out.println();
    }
  }
}
/*
For a maximum of 7 games:

     0       1       2      3      4
     ---------------------------------
0    0.5     0.34375 0.1875 0.0625 0.0 
1    0.65625 0.5     0.3125 0.125  0.0 
2    0.8125  0.6875  0.5    0.25   0.0 
3    0.9375  0.875   0.75   0.5    0.0 
4    1.0     1.0     1.0    1.0    x

For a maximum of 9 games:

     0          1          2         3        4       5
     ----------------------------------------------------
0    0.5        0.36328125 0.2265625 0.109375 0.03125 0.0 
1    0.63671875 0.5        0.34375   0.1875   0.0625  0.0 
2    0.7734375  0.65625    0.5       0.3125   0.125   0.0 
3    0.890625   0.8125     0.6875    0.5      0.25    0.0 
4    0.96875    0.9375     0.875     0.75     0.5     0.0 
5    1.0        1.0        1.0       1.0      1.0     x

*/
