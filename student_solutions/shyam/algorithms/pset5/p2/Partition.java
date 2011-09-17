/*
  Partition.java
  Algorithms: PSet 5 - Problem 2
  Shyam Visweswaran
*/
import java.util.Vector;

public class Partition
{
  // parent value int the table array
  private static final int NONE = 0;
  private static final int UP = 1;
  private static final int LEFT = 2;
  
  private static int[] input; // first value is a dummy so that the first element is at index 1
  private static int[][] table; // stores parentage of the cell; has a zeorth row and zeroth column
  private static Vector a; // to store elements of one set
  private static Vector b; // to store elements of other set

  public static void main(String[] args)
  {
    if (args.length > 0)
    { // command line input parsed into input array
      input = new int[args.length + 1];
      input[0] = -1000;
      for (int i = 1; i < input.length; i++)
        input[i] = Integer.parseInt(args[i - 1]);
    }
    else
    { // votes array is used as default; note dummy first variable
      input = new int [] 
      { -1000,
        3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,
        5,5,5,5,
        6,6,
        7,7,7,
        8,8,8,8,8,8,
        9,9,
        10,10,
        11,11,11,11,
        12,12,
        13,13,
        14,15,18,21,22,23,25,32,33,54 };
    }

    int sum = 0;
    for (int i = 1; i < input.length; i++)
      sum += input[i];
    if ((sum % 2) != 0)
    { // if sum is odd problem cannot be done
      System.out.println("Sum is odd - hence cannot partition");
      System.exit(0);
    }
    table = new int[input.length][(sum / 2) + 1];
    computeParents(input, table); 
    
    if (table[table.length - 1][table[0].length - 1] == 0)
    { // if the last cell of table has no parent and hence unreachable
      System.out.println("No solution");
      System.exit(0);
    }

    a = new Vector();
    b = new Vector();    
    makeSets(table, a, b); // traverse path int table and sort elements into two Vectors
    displaySet(a); // display first vector and its sum
    System.out.println();
    displaySet(b); // display second vector and its sum
    System.out.println();
  }
  
  // create a parentage table
  public static void computeParents(int[] input, int[][] table)
  {
    for (int i = 0; i < table.length; i++)
    {
      for (int j = 0; j < table[i].length; j++)
      { // cell[0][0] is set to UP
        if (j == 0) table[i][j] = UP; // first column set to UP
        else if (i == 0) table[i][j] = NONE; // first row (except first cell) set to NONE
        else if (((j - input[i]) >= 0) && ((table[i-1][j - input[i]]) != 0)) table[i][j] = LEFT;
        else if (table[i-1][j] != 0) table[i][j] = UP;
        else table[i][j] = NONE;
        
      }
    }
  }
  
  // traverse parentage table from the last cell and sort elements into 2 Vectors
  public static void makeSets(int[][] table, Vector a, Vector b)
  {
    int j = table[0].length - 1;
    for (int i = table.length - 1; i > 0; i--)
    {
      if (table[i][j] == LEFT)
      {
        j = j - input[i];
        a.add(new Integer(input[i]));
      }
      else if (table[i][j] == UP)
      {
        b.add(new Integer(input[i]));
      }
    }
  }

  // display vector int reverse and sum its contents
  public static void displaySet(Vector v)
  {
    int total = 0;
    System.out.print("Set: ");
    for (int i = v.size() - 1; i >= 0; i--)
    {
      System.out.print(v.get(i).toString() + " ");
      total += ((Integer)v.get(i)).intValue();
    }
    System.out.println(" Total: " + total);
  }
}

/*
Partition output for the electoral votes input.
A tie is possible and a possible solution is for the 2 sets is:

Set: 12 14 15 18 21 22 23 25 32 33 54  Total: 269

Set: 3 3 3 3 3 3 3 3 4 4 4 4 4 4 5 5 5 5 6 6 7 7 7 8 8 8 8 8 8 9 9 10 10 11 11 11 11 12 13 13  Total: 269
*/





   
