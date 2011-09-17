/* =======================================================
 * Matcher.java - Approximate string matching
 * Algorithms: PSet 5 - Problem 7
 * Shyam Visweswaran
 */

public class Matcher
{ // default text and search strings for testing
  private static String search = "MALPQ";
  private static String text = "TRGHKLMNOPQWYQWKKKURMJLOQOP";
  
  public static void main(String[] args)
  {
    if (args.length == 2)
    {
      search = args[0];
      text = args[1];
    }
    runMatch(text, search);
  }
  
  private static void runMatch(String text, String search)
  {
    int[][] table = new int[search.length() + 1][text.length() + 1];
    for (int i = 0; i < table.length; i++)
    {
      for (int j = 0; j < table[0].length; j++)
      {
        if (i == 0) table[i][j] = 0; // first row is all zero
        else if (j == 0) table[i][j] = i; // first column is set to the row index
        else
        {
          int p = 1;
          if ((search.substring(i - 1, i)).equals(text.substring(j - 1, j))) p = 0;
          table[i][j] = getMin(table[i - 1][j] + 1, table[i][j - 1] + 1, table[i - 1][j - 1] + p);
        }
      }
    }

    // the answer is in the last row; so check last row for the minimum value
    int minValue = 1000000;
    for (int j = 1; j < table[0].length; j++)
      if (table[table.length - 1][j] < minValue)
        minValue = table[table.length - 1][j];

    System.out.println(text + " <-- text string");

    // search the last row for the minimum value and the corresponding j value
    // is the position where the search string ends int the text string
    // Print all matches that correspond to the minimum value
    int count = 0;
    for (int j = search.length(); j < table[0].length; j++)
    {
      if (table[table.length - 1][j] == minValue)
      {
        String space = "";
        for (int a = 0; a < (j - search.length()); a++)
          space += " ";
        System.out.println(space + search);
        count++;
      }
    }
    System.out.println(count + " matches found");
    System.out.println("The search string differs by " + minValue + " characters fromm the matches");
  }
  
  // utility for computing minimum of 3 values
  private static int getMin(int a, int b, int c)
  {
    int min = a;
    if (b < min) min = b;
    if (c < min) min = c;
    return min;
  }
}                
/*
Test with default search and text strings:

TRGHKLMNOPQWYQWKKKURMJLOQOP <-- text string
      MALPQ
                    MALPQ
2 matches found
The search string differs by 2 characters fromm the matches
*/
