/*
  Algorithms PSet-1 Problem 3
  Shyam Visweswaran
  Binary Search
*/

public class BinSearch
{
  public static void main(String[] args)
  { 
    int target = Integer.parseInt(args[0]); // parse the input for target
    int[] anArray = new int[100]; // initialize an array of 100
    for (int i = 0; i < 21; i++) anArray[i] = (i+1)*3; // fill the first 20 spots with multiples of 3 starting at 3
    for (int i = 20; i < 100; i++) anArray[i] = 0; // last 80 spots filled with 0
    System.out.println(expand(anArray, 0, 0, target));
  }

  // find appropiate target interval by doubling interval size
  // till end of interval hits 0 or is less than target; when that
  // happens call shrink
  public static int expand(int[] a, int first, int last, int target)
  {
    int size = last - first + 1;
    if (a[last] == 0) return shrink(a, first, size, target); // we are beyond array end
    else if (a[last] == target) return last;
    else if (a[last] < target) return expand(a, last, (last + size*2 - 1), target);
    else return shrink(a, first, size, target);
  }
  
  // this is regular binary search which we call once we know the starting point of
  // the interval and the size of the interval
  public static int shrink(int[] a, int first, int size, int target)
  {
    int middle;
    if (size <= 0) return -1; // return -1 if element not found
    else if (a[first + size - 1] == 0)  return shrink(a, first, size/2, target);
    else
    {
      middle = first + size/2;
      if (target == a[middle]) return middle;
      else if (target < a[middle])  return shrink(a, first, size/2, target);
      else return shrink(a, middle+1, (size-1)/2, target);
    }
  }
}

/*
  Test runs:
  java BinSearch 21 --> 6
  java Binsearch 57 --> 18
  java BinSearch 2 --> -1
*/
