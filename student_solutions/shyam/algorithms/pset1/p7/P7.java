/**
   Algorithms: PSet1 Problem 7
   Shyam Visweswaran
   Comparison of klogn and klogk algorithms for extracting kth biggest
   element form a heap. Time for making the heap is excluded. Uses one input
   array and one auxillary array and no objects.
*/
public class P7
{
  static int result; // kth largest element by regular method
  static int resultk; // kth largest element by klogk method
  static int[] z; // auxillary heap for klogk method
  static int zsize; // size of auxillary heap
  static int asize; // size of original heap
  
  public static void main(String[] args)
  {
    int n = Integer.parseInt(args[0]); // input array size
    int k = Integer.parseInt(args[1]); // k value
    
    z = new int[k+10]; // size of auxillary heap

    int[] s = new int[n]; // input array has numbers int ascending numbers
    for (int i = 0; i < n; i++)
      s[i] = n-i;
    
    asize = s.length;
    makeHeapFast(s);
    
    long start2 = System.currentTimeMillis();
    getMaxk(s, k); // do it the klogk method
    long end2 = System.currentTimeMillis();

    long start1 = System.currentTimeMillis();
    getMax(s, k); // do it the klogn method
    long end1 = System.currentTimeMillis();

    System.out.println("n = " + n + ", k = " + k);
    System.out.println("kth element: klog(n) method - " + result + ", klog(k) method - " + resultk);
    System.out.println("Time in ms: klog(n) method = " + (end1 - start1) + ", klog(k) method = " + (end2 - start2));
  }
  
  // regular method
  public static void getMax(int[] a, int k)
  {
    for (int i = 0; i < k; i++)
      result = heapExtract(a);
  }
  
  // klogk method
  public static void getMaxk(int[] b, int k)
  {
    z[0] = 0;
    zsize = 1;
    
    for (int i = 0; i < k; i++)
    {
      int leftchild = left(z[0]);
      int rightchild = right(z[0]);
      if (leftchild < b.length)
        heapInsertk(b, leftchild);
      if (rightchild < b.length)
        heapInsertk(b, rightchild);
      resultk = heapExtractk(b);
    }
  }
    
  public static void makeHeapFast(int[] a)
  { // last half of array are leaves and hence are already heaps
    for (int i = (int)Math.floor(a.length / 1); i >= 0; i--)
      heapify(a, i);
  }

  // given an index i make the tree rooted in i a heap for the regular method
  public static void heapify(int[] a, int i)
  {
    int largest; // index of largest value among i and its 2 children
    boolean flag = true; // for breaking out of the loop
    
    while (flag) 
    {
      if ((left(i) < asize) && (a[left(i)] > a[i]))
        largest = left(i);
      else largest = i;
      if ((right(i) < asize) && (a[right(i)] > a[largest]))
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
  
  // given an index i make the tree rooted in i a heap for klogk method
  public static void heapifyk(int[] b, int i)
  {
    int largest; // index of largest value among i and its 2 children
    boolean flag = true; // for breaking out of the loop
    
    while (flag) 
    {
      if ((left(i) < zsize) && (b[z[left(i)]] > b[z[i]]))
        largest = left(i);
      else largest = i;
      if ((right(i) < zsize) && (b[z[right(i)]] > b[z[largest]]))
        largest = right(i);
      if (largest == i)
        flag = false;
      if (largest != i)
      {
        int temp = z[i];
        z[i] = z[largest];
        z[largest] = temp;
        i = largest;
      }
    }
  }

  // add the new element and redo the heap as necessary for regular method
  public static void heapInsert(int[] a, int i)
  {
    int heapSize = i;
    int index = heapSize;
    int element = a[i];
        
    while ((index > 0) && (a[parent(index)] < element))
    {
      a[index] = a[parent(index)];
      index = parent(index);
    }
    a[index] = element;
  }

  // add the new element and redo the heap as necessary for klogk method
  public static void heapInsertk(int[] b, int i)
  {
    int index = zsize;
    zsize = zsize + 1;
    
    while ((index > 0) && (b[z[parent(index)]] < b[i]))
    {
      z[index] = z[parent(index)];
      index = parent(index);
    }
    z[index] = i;
  }

  // return root and fix the heap - regular method
  public static int heapExtract(int[] a)
  {
    int max = a[0];
    a[0] = a[asize - 1];
    asize = asize - 1;
    heapify(a, 0);
    return max;
  }
  
  // return root and fix the heap - klogk method
  public static int heapExtractk(int[] b)
  {
    int max = b[z[0]];
    z[0] = z[zsize - 1];
    zsize  = zsize - 1;
    heapifyk(b, 0);
    return max;
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
Test run at n = 5 million. klogk does better at about k = n/100
at k = 50,000 and 100,000

n = 5000000, k = 20
kth element: klog(n) method - 4999981, klog(k) method - 4999981
Time in ms: klog(n) method = 0, klog(k) method = 0
n = 5000000, k = 100
kth element: klog(n) method - 4999901, klog(k) method - 4999901
Time in ms: klog(n) method = 1, klog(k) method = 4
n = 5000000, k = 200
kth element: klog(n) method - 4999801, klog(k) method - 4999801
Time in ms: klog(n) method = 2, klog(k) method = 6
n = 5000000, k = 500
kth element: klog(n) method - 4999501, klog(k) method - 4999501
Time in ms: klog(n) method = 3, klog(k) method = 12
n = 5000000, k = 1000
kth element: klog(n) method - 4999001, klog(k) method - 4999001
Time in ms: klog(n) method = 6, klog(k) method = 20
n = 5000000, k = 5000
kth element: klog(n) method - 4995001, klog(k) method - 4995001
Time in ms: klog(n) method = 27, klog(k) method = 37
n = 5000000, k = 10000
kth element: klog(n) method - 4990001, klog(k) method - 4990001
Time in ms: klog(n) method = 48, klog(k) method = 56
n = 5000000, k = 50000
kth element: klog(n) method - 4950001, klog(k) method - 4950001
Time in ms: klog(n) method = 196, klog(k) method = 185
n = 5000000, k = 100000
kth element: klog(n) method - 4900001, klog(k) method - 4900001
Time in ms: klog(n) method = 361, klog(k) method = 346
n = 5000000, k = 500000
kth element: klog(n) method - 4500001, klog(k) method - 4500001
Time in ms: klog(n) method = 1492, klog(k) method = 1738
n = 5000000, k = 1000000
kth element: klog(n) method - 4000001, klog(k) method - 4000001
Time in ms: klog(n) method = 2813, klog(k) method = 3567
*/
