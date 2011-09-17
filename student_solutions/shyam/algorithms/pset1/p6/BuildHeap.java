/**
   Algorithms: PSet1 Problem 6
   Shyam Visweswaran
   BuildHeap with Heapify (Fast) and with HeapInsert (Slow) - both iterative
*/
public class BuildHeap
{
  public static void main(String[] args)
  { // parse command line array; run the slow and fast BuildHeap methods
    // and return heaps and running times
    if (args.length > 0)
    {
      int[] f = new int[args.length];
      int[] s = new int[args.length];
      for (int i = 0; i < args.length; i++)
      {
        f[i] = Integer.parseInt(args[i]);
        s[i] = f[i];
      }
      
      long startf = System.currentTimeMillis();
      makeHeapFast(f);
      long endf = System.currentTimeMillis();

      for (int i = 0; i < f.length; i++)
        System.out.print(f[i] + " ");
      System.out.println();
      System.out.println("Fast time in ms: " + (endf - startf));

      long starts = System.currentTimeMillis();
      makeHeapSlow(s);
      long ends = System.currentTimeMillis();

      for (int i = 0; i < s.length; i++)
        System.out.print(s[i] + " ");
      System.out.println();
      System.out.println("Slow time in ms: " + (ends - starts));
    }
    // if no comand line arguments; then run the following test to
    // see which method runs faster. Run both methods on input sizes of multiples
    // of 100000 (1 through 9) and output the time ratios (ith input / 1st input)
    // for each method
    else
    {
      int first = 100000;
      int runs = 6;
      
      int[] fastArray = new int[0];
      int[] slowArray = new int[0];
      long[] timeFast = new long[runs];
      long[] timeSlow = new long[runs];
            
      for (int i = 0; i < runs; i++)
      {
        fastArray = new int[first * i];
        slowArray = new int[first * i];
        for (int j = 0; j < fastArray.length ; j++)
        {
          //fastArray[j] = (int)(Math.random() * 1000);
          fastArray[j] = j;
          slowArray[j] = fastArray[j];
        }
        
        long startFast = System.currentTimeMillis();
        makeHeapFast(fastArray);
        long endFast = System.currentTimeMillis();
        timeFast[i] = endFast - startFast;

        long startSlow = System.currentTimeMillis();
        makeHeapSlow(slowArray);
        long endSlow = System.currentTimeMillis();
        timeSlow[i] = endSlow - startSlow;
      }

      for (int i = 1; i < runs; i++)
      {
        System.out.print("Size: " + (first * i) + " (" + i + ") ");
        System.out.print("Fast: " + timeFast[i] + " (" + (float)timeFast[i]/timeFast[1] + ") ");
        System.out.println("Slow: " + timeSlow[i] + " (" + (float)timeSlow[i]/timeSlow[1] + ") ");        
      }
    }
  }
  

  public static void makeHeapFast(int[] a)
  { // last half of array are leaves and hence are already heaps
    for (int i = (int)Math.floor(a.length / 2); i >= 0; i--)
      heapify(a, i);
  }

  public static void makeHeapSlow(int[] b)
  { // insert one element at a time
    for (int i = 1; i < b.length; i++)
      heapInsert(b, i);
  }

  // given an index i make the tree rooted in i a heap
  public static void heapify(int[] a, int i)
  {
    int largest; // index of largest value among i and its 2 children
    boolean flag = true; // for breaking out of the loop
    
    while (flag) 
    {
      if ((left(i) < a.length) && (a[left(i)] > a[i]))
        largest = left(i);
      else largest = i;
      if ((right(i) < a.length) && (a[right(i)] > a[largest]))
        largest = right(i);
      if (largest == i)
        flag = false;
      if (largest != i)
      {
        int temp = a[i];
        a[i] = a[largest];
        a[largest] = temp;
        i = largest;
      }
    }
  }
  
  // add the new element and redo the heap as necessary
  public static void heapInsert(int[] b, int i)
  {
    int heapSize = i;
    int index = heapSize;
    int element = b[i];
        
    while ((index > 0) && (b[parent(index)] < element))
    {
      b[index] = b[parent(index)];
      index = parent(index);
    }
    b[index] = element;
  }

  // 3 utility methods to compute parent and children
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
Input 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
Output 
Fast method (Heapify) 15 11 14 9 10 13 7 8 4 2 5 12 6 3 1 
Slow method (HeapInsert) 15 10 14 7 9 11 13 1 4 3 8 2 6 5 12 

Output using random numbers (random*1000)

Size: 10000 (1) Fast: 19 (1.0) Slow: 29 (1.0) 
Size: 20000 (2) Fast: 11 (0.57894737) Slow: 43 (1.4827586) 
Size: 30000 (3) Fast: 8 (0.42105263) Slow: 61 (2.1034484) 
Size: 40000 (4) Fast: 10 (0.5263158) Slow: 81 (2.7931035) 
Size: 50000 (5) Fast: 12 (0.6315789) Slow: 103 (3.5517242) 
Size: 60000 (6) Fast: 15 (0.7894737) Slow: 124 (4.275862) 
Size: 70000 (7) Fast: 18 (0.94736844) Slow: 144 (4.965517) 
Size: 80000 (8) Fast: 22 (1.1578947) Slow: 165 (5.6896553) 
Size: 90000 (9) Fast: 25 (1.3157895) Slow: 185 (6.37931)

Size: 100000 (1) Fast: 37 (1.0) Slow: 220 (1.0) 
Size: 200000 (2) Fast: 60 (1.6216216) Slow: 415 (1.8863636) 
Size: 300000 (3) Fast: 88 (2.3783784) Slow: 619 (2.8136363) 
Size: 400000 (4) Fast: 118 (3.1891892) Slow: 825 (3.75) 
Size: 500000 (5) Fast: 147 (3.9729729) Slow: 1032 (4.690909) 
Size: 600000 (6) Fast: 177 (4.783784) Slow: 1241 (5.640909) 
Size: 700000 (7) Fast: 207 (5.5945945) Slow: 1445 (6.568182) 
Size: 800000 (8) Fast: 236 (6.3783784) Slow: 1654 (7.518182) 
Size: 900000 (9) Fast: 266 (7.189189) Slow: 1857 (8.440909)
 
Size: 1000000 (1) Fast: 307 (1.0) Slow: 2097 (1.0) 
Size: 2000000 (2) Fast: 598 (1.9478828) Slow: 4136 (1.9723414) 
Size: 3000000 (3) Fast: 898 (2.9250815) Slow: 6239 (2.9752026) 
Size: 4000000 (4) Fast: 1202 (3.9153094) Slow: 8703 (4.1502147) 
Size: 5000000 (5) Fast: 1502 (4.892508) Slow: 10462 (4.989032) 
*/
