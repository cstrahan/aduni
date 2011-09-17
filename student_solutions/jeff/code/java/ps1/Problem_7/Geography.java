/**
   A first attempt at a geographical heirarchy.
   This one is very much oriented toward an object keeping track of
   its child classes, to the extent that there are calls to constructor
   methods within some classes to create classes below it.

   While this might be useful within a databasing context, I am unsure if
   this class modelling would be most effective in the long run.

   Each class has an array to keep track of its child objects.

   @author Jeffrey Radcliffe
   @version 0.1
*/
public abstract class MapItem
{
  /**
     Retrieves the objects id number
     @return int id number
  */
  public int getId() {}

  // fields
  private int id;
}

public class Country extends MapItem
{
  // constructors
  Country() {}
  Country(String name) {}
  Country(String name, String capitol) {}
  Country(String name, String capitol, double area) {}
  Country(String name, String capitol, double area, String[] neighbors) {}
    
  // methods
  public String getCities() {}
  public double area() {}
  public String capitol() {}
  public neighbors() {}
  addNeighbor() {}
  setCapitol() {}
  public void addState() {}
  addBoundry() {}
  addRiver() {}                 // assuming a country can own a river
  public String toString() {}
  
  // fields
  private double area;
  private String capitolName;
  private LinkedList states;        
  private static int numberOfCountries;
}

	
public class State extends Country
{
  // methods
  public String getCountry() {}
  public String toString() {}
  public void addState() {}
  addCity() {}
  
  // fields
  private String capitolName;
  private static int numberOfStates;
}

public class City extends State
{
  // constructors
  City() {}
  City(String name) {}
  City(String name, 
  
  // methods
	public double distance(City aCity) {}
  public String toString() {}

  // fields
  private static int numberOfCities;
}

public class BoundrySegment extends Thing
{
	// constructors
  BoundrySegment() {}
  BoundrySegment(String name) {}
  BoundrySegment(String name, float length) {}

  // methods 
  borderOf() {}
  boundryLength() {}
  toString() {}

  // fields
  private static int numberOfBoundries;
}

public class River extends BoundrySegment
{
  // constructors
  River() {}
  River(String name) {}
  River(String name, float length) {}

  // method
  toString() {}

  // fields
  private static int numberOfRivers;
}
      
