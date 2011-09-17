public class MapDriver
{
  public static void main(String[] args)
  {
    Country myCountry = new Country("Elvistopia","Graceland");

    System.out.println(myCountry);
    System.out.println("Capitol is of " + myCountry.getName() +
                       " is " + myCountry.capitol());
    
  }
}
