/**
   Algorithms: PSet1 Problem 2
   Shyam Visweswaran
   MaxSort
*/

public class MaxSort
{
  public static void main(String[] args)
  { // read values from input args and store it in sortArray
    int[] sortArray = new int[args.length];
    for (int i = 0; i < args.length; i++)
      sortArray[i] = Integer.parseInt(args[i]);

    sort(sortArray); // call sort with the array

    for (int i = 0; i < sortArray.length; i++) // display sorted array
      System.out.print(sortArray[i] + " ");
    System.out.println();
  }
  
  public static void sort(int[] sortArray)
  { // lastLocation is where the maximum element will be swapped into
    for (int lastLocation = (sortArray.length - 1); lastLocation >= 0; lastLocation--)
    {
      int maxLocation = 0; // index of the biggest element so far
      for (int i = 0; i <= lastLocation; i++)
        if (sortArray[i] > sortArray[maxLocation]) maxLocation = i;
      // swap element in maxLocation with that in lastLocation
      int temp = sortArray[maxLocation];
      sortArray[maxLocation] = sortArray[lastLocation];
      sortArray[lastLocation] = temp;
    }
  }
}

/*
  Test run:
  java MaxSort 45 89 0 21 6 -8 -10 11 32 101
  -10 -8 0 6 11 21 32 45 89 101
*/
      
    
