/**
   BubbleSort.java
   A simple class to do bubble sorting

   @author JMR
   @date Fri Feb  2, 2001  8:27 AM
*/

public class BubbleSort{
  public static void bubbleSort(int[] array) {
    BOUND = (array.length - 1); 
    boolean done = false;
    boolean switchMade;
    for(int i = 0; i < BOUND && !done; i++) {
      switchMade = false;
      for(int j = 0; j < BOUND; j++) {
        if(array[j] > array[j+1]) {
          int temp = array[j+1];
          array[j+1] = array[j];
          array[j] = temp;
          switchMade = true;
        }
      }
      passes++;
      if(verbose) display(array); 
      if(!switchMade)
        done = true;
    }
  }    
  
  public static void display(int[] array) {
    for(int i = 0; i < array.length; i++)
      System.out.print(array[i] + " ");
    System.out.println();
  }

  public static void main(String[] args) {
    // initialize some test arrays
    int[] array2 = 
      { 503, 87, 512, 61, 908, 170, 897, 275, 653, 426, 154, 509,
      612,677, 765, 703 };
    int[] array =
    { 12, 10, 3, 37, 57, 2, 23, 9 };

    
    display(array2);
    bubbleSort(array2);
    display(array2);
  }

  // fields
  static int BOUND;
  static int passes = 0;
  static boolean done = false;
  static boolean verbose = true;
}
