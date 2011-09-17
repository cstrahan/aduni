/**
   Hello.java -- first try is as charm
*/
public class Hello
{
  public static void main(String[] args)
  {

    System.out.println("Obligatory message:" + getHello());

    System.out.println();
    System.out.println("Recursive factorial of 5 is " + rFact(5));

    System.out.println();
    System.out.println("Iterative factorial of 5 is " + iFact(5));
  }

  // yes, again...
  public static String getHello()
  {
    String string = "Howdy terrans!";
    return (string);
  }

  // recursive factorial method
  public static int rFact(int n)
  {
    int number = n;
    if(number == 1)
      return 1;
    else
      return (number  * rFact(number - 1));
  }

  // iterative factorial method
  public static int iFact(int n)
  {
    int number = n;
    int val = 1;
    
    for(int i = number; i > 0 ; i--)
      {
        val = val * i;
      }
    return val;
  }
}
