/**
   Algorithms: PSet1 Problem 10
   Shyam Visweswaran
   BuildHeap for d-ary heaps - comparison of sort times for various d
*/
public class BuildDHeap
{
  static int d;
  static int[] a;
  static int asize;
  
  public static void main(String[] args)
  { // parse command line array; run the slow and fast BuildHeap methods
    // and return heaps and running times
    if (args.length > 1)
    {
      a = new int[args.length - 1];
      d = Integer.parseInt(args[0]);
      asize = (args.length - 1);
    
      for (int i = 1; i < args.length; i++)
        a[i - 1] = Integer.parseInt(args[i]);
      
      long start = System.currentTimeMillis();
      sortHeap();
      long end = System.currentTimeMillis();

      System.out.println("d = " + d + ", n = " + a.length);
      for (int i = 0; i < a.length; i++)
        System.out.print(a[i] + " ");
      System.out.println();
      System.out.println("Time in ms: " + (end - start));
    }
    else
    {
      int inputsize = Integer.parseInt(args[0]);
      int[] stuff = new int[inputsize];
      for (int i = 0; i < inputsize; i++)
        stuff[i] = i;
      
      // first do d = 2 with optimized binary code
      a = new int[inputsize];
      for (int j = 0; j < inputsize; j++)
        a[j] = stuff[j];
      d = 2;
      asize = inputsize;
      long startb = System.currentTimeMillis();
      sortHeapbinary();
      long endb = System.currentTimeMillis();
      System.out.println("n = " + inputsize + ", d = " + d + " (optimized), Time = " + (endb - startb));
      
      // now do d = 2 to 16 with generic code
      for (int i = 2; i < 17; i++)
      {
        a = new int[inputsize];
        for (int j = 0; j < inputsize; j++)
          a[j] = stuff[j];
        d = i;
        asize = inputsize;
        long start = System.currentTimeMillis();
        sortHeap();
        long end = System.currentTimeMillis();
        System.out.println("n = " + inputsize + ", d = " + d + ", Time = " + (end - start));
      }
    }
  }
    


  /*
    Following is generic code for d-ary trees
  */
  public static void sortHeap()
  {
    makeHeap();
    for (int i = (a.length - 1); i >= 1; i--)
    {
      int temp = a[0];
      a[0] = a[asize - 1];
      a[asize - 1] = temp;
      asize = asize - 1;
      heapify(0);
    }
  }
  
  public static void makeHeap()
  {
    for (int i = (asize - 1); i >= 0; i--)
      heapify(i);
  }

  // given an index i make the tree rooted in i a heap
  public static void heapify(int i)
  {
    int[] child = getChildren(i);
    int largest = i;
    
    for (int j = 0; j < child.length; j++)
      if ((child[j] < asize) && (a[child[j]] > a[largest])) largest = child[j];
    
    if (largest != i)
    {
      int temp = a[i];
      a[i] = a[largest];
      a[largest] = temp;
      heapify(largest);
    }
  }
    
  // 2 utility methods to compute parent and children
  public static int getParent(int i)
  {
    int k = i + 1;
    return (int)Math.floor(k + (k % d)) - 1;
  }

  public static int[] getChildren(int i)
  {
    int[] child = new int[d];
    int middle = (i + 1) * d;
    for (int j = 0; j < d; j++)
      child[j] = ((middle - (int)Math.floor(d / 2)) + j) - 1;
    return child;
  }

  /*
    The following is optimized code for binary trees
  */
  public static void sortHeapbinary()
  {
    makeHeapbinary();
    for (int i = (a.length - 1); i >= 1; i--)
    {
      int temp = a[0];
      a[0] = a[asize - 1];
      a[asize - 1] = temp;
      asize = asize - 1;
      heapifybinary(0);
    }
  }

  public static void makeHeapbinary()
  {
    for (int i = (asize - 1); i >= 0; i--)
      heapifybinary(i);
  }

  public static void heapifybinary(int i)
  {
    int largest;
    
    if ((left(i) < a.length) && (a[left(i)] > a[i]))
      largest = left(i);
    else largest = i;
    if ((right(i) < a.length) && (a[right(i)] > a[largest]))
      largest = right(i);
    
    if (largest != i)
    {
      int temp = a[i];
      a[i] = a[largest];
      a[largest] = temp;
      heapifybinary(largest);
    }
  }
  
  public static int parent(int i)
  {
    int j = i + 1;
    int k = (int)Math.floor(j / 2);
    return k - 1;
  }

  public static int left(int i)
  {
    return (((i + 1) * 2) - 1);
  }

  public static int right(int i)
  {
    return ((((i + 1) * 2) + 1) - 1);
  }
}

/*
Sorting with d-ary heaps with ascending order numbers
The optimized d = 2 is the fastest and uses 2 if-else statements
for comparisons. The generic comparison routine for d = 2 is much
slower. But using the generic comparison routine shows that d = 3 - 8
are faster than d = 2

n = 10000, d = 2 (optimized), Time = 75
n = 10000, d = 2, Time = 178
n = 10000, d = 3, Time = 156
n = 10000, d = 4, Time = 171
n = 10000, d = 5, Time = 141
n = 10000, d = 6, Time = 187
n = 10000, d = 7, Time = 145
n = 10000, d = 8, Time = 159
n = 10000, d = 9, Time = 214
n = 10000, d = 10, Time = 224
n = 10000, d = 11, Time = 191
n = 10000, d = 12, Time = 169
n = 10000, d = 13, Time = 175
n = 10000, d = 14, Time = 199
n = 10000, d = 15, Time = 221
n = 10000, d = 16, Time = 253

n = 100000, d = 2 (optimized), Time = 445
n = 100000, d = 2, Time = 2019
n = 100000, d = 3, Time = 1928
n = 100000, d = 4, Time = 2030
n = 100000, d = 5, Time = 1997
n = 100000, d = 6, Time = 1716
n = 100000, d = 7, Time = 2022
n = 100000, d = 8, Time = 1643
n = 100000, d = 9, Time = 2410
n = 100000, d = 10, Time = 2573
n = 100000, d = 11, Time = 2056
n = 100000, d = 12, Time = 1788
n = 100000, d = 13, Time = 2122
n = 100000, d = 14, Time = 2501
n = 100000, d = 15, Time = 3025
n = 100000, d = 16, Time = 3651
*/  
  

