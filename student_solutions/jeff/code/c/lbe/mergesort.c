g/* the function copy takes one argument and  copies it to another */
void copy(int *from, int *to, int n)
{
  while (n--)            // checks for terminal condition
    *to++ = *from++;     // moves the contents of location from to location to
}

/* procedure merge, which takes 5 arguments, 2 for the first half
   2 for the second half and the output array (array_c). This merges
   the contents of the first pile with the second
*/
void merge(int *array_a, int n_a, int *array_b, int n_b, int *array_c)
{
  while (n_a && n_b)   // while n_a and n_b are both positive (i.e. stuff in both piles)
    {
      if (*array_a > *array_b) //checks to see if array_a is larger
	{
	  *array_c++ = *array_b++;  //sticks the value from array_b into array_c
	  n_b--;                    //decrements n_b
	}
      else
	{
	  *array_c++ = *array_a++;  //sticks the value from array_a into array_c
	  n_a--;                    //decrements n_a
	}
    }
  /* at this point, either n_a or n_b is zero */
  while (n_a)       // if n_a is still positive
    {
      *array_c++ = *array_a++;      //places the value from array_a into array_c
      n_a--;                        //decrements n_a
    }
  while (n_b)       // if n_b is still positive
    {
      *array_c++ = *array_b++;        //places the value from array_b into array_c
      n_b--;                          //decrements n_b
    }
}
/* this is the actual sorting procedure, which will recurse on itself until there are only piles of
   1. Then it will build up the sorted array into array_c, and finally will copy the results back into
   array.
*/
void sort(int *array, int n)  // procedure sort, which takes two arguments
{
  int *array_a, *array_b, *array_c;  // defining variables
  int n_a, n_b;
 
  if (n > 1)
    {
      array_a = array;        //sets array equal to array_a
      n_a = n / 2;            // sets n_a to half of n
      
      array_b = array + n_a;  // array_b gets the value of array + n_a 
      n_b = n - n_a;          // n_b gets the value of n_b - n (i.e. the rest of the stuff)
      sort (array_a, n_a);    // recurs on the first half (using array_a and n_a)
      sort (array_b, n_b);    // recurs on the second half (using array_b and n_b)

      array_c = allocate(n);  // gives array_c enough space to do the merge

      merge (array_a, n_a, array_b, n_b, array_c); // performing the merge, results in array_c

      copy (array_c, array, n); // copies the value array_c (which has n elements) to array 
    }
}




