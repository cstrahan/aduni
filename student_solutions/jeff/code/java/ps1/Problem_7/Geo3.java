/**
   Geo3.java

   My third attempt at a coherent data model, using more interfaces.
   @author Jeffrey M. Radcliffe

   Created: Sun Jan  7, 2001  3:10 PM
*/

/**
   The building block of the Geography system
*/
public class Point
{
  private double x;
  private double y;
  /**
     @return Value of x (latitude)

  */
  public double getX() {}

  /**
     @return Value of y (longitude)
  */
  public double getY() {}
}

abstract class GeographyTools
{
  public double calculateDistance(Point a, Point b) {}
  // Other useful geographical methods ...
}

abstract class Line extends GeographyTools
{
  private Point startPoint;
  private Point endPoint;

  /**
     Calculates the length of the line
     @return The length
  */
  public double getLength() {}
}

public class Border extends Line
{
  private String[] borderItems; // Tokenized array containing typed neighbor
                                // Infomation, i.e. "State:Montana"
  /**
     Gets the items that this object borders
     @return A String containing the name and type of the bordering objects
  */
  public String borderOf() {}
}

/**
   This sort of river does not border a region, but rather
   passes through a number of regions
*/
public class River extends Line
{
  private String[] riverItems;

  /**
     the regions this river passes through
     @return String containing regions
  */
  public String passesThrough() {}
}

/**
   This sort of river behaves like a river but also borders regions
   (O! For multiple inheritance!)
*/
public class RiverBorder extends Border
{
  private String[] riverItems;

  /**
     The regions this river passes through
     @return String containing regions
  */
  public String passesThrough() {}
}

/**
   This class is the base class for all bounded regions
*/
abstract class Region extends GeographyTools
{
  private Point center;         // Center of the region
  private double area;          // Area of the region
  private String[] boundries;   // The items that are on the border of region
  private String[] neighbors;   // Regions sharing a border with this region

  /**
     Returns the region's neighbors
     @return Neighbors for region as a String
  */
  public String neighbors() {}

  /**
     Gets the region's area
     @return The Area
  */
  public double getArea() {}

  /**
     Calclutes the perimiter from the combined lengths of <code>boundries</code>.
     @return The Perimeter
  */
  public double getPerimeter() {}
  
  /**
     Calculates (using calculateDistance from GeographyTools) the distance from
     the current region to another region
     @param otherRegion The other region
     @return the distance
  */
  public double getDistance(Region otherRegion) {}
  /**
     Gets the name of the region
     @return The name of the region
  */
  abstract String getName();
}
  
interface PoliticalRegion
{
  // Some examples of a political interface (not implemented below!)
  public double getPopulation();
  public double getProduction();
  public String getLeader();
  public String getLanguage();
  // And more!
}
interface TopographicalRegion
{
  // Interface specifics...
}

public class Country extends Region implements PoliticalRegion
{
  private String countryName;
  private String countryCapitol;
  private String[] states;      // Array of the states within the country

  public String getName() {}

  /**
     Gets the country capitol
     @return The country capitol
  */
  public String getCapitol() {}
  /**
     Gets the states contained within the country
     @return A string containing the country's states
  */
  public String getStates() {}
}

public class State extends Region implements PoliticalRegion
{
  private String stateName;
  private String capitolName;
  private String countryName;
  
  private String[] cities;      // Array of the cities withing the state

  public String getName() {}
  /**
     Gets the state capitol
     @return The state capitol
  */
  public String getCapitol() {}
  public String getCountry() {}

  /**
     Gets the cities contained within the state
     @return A string containing the country's cities
  */
  public String getCities() {}
}
  
public class City extends Region implements PoliticalRegion
{
  private String cityName;
  private String stateName;
  private String countryName;

  public String getCapitol() {}

  /**
     gets the name of the city
     @return The city name
   */
  public String getName() {}

  /**
     Gets the name of the country to which this city belongs
     @return The country name
  */
  public String getCountry() {}

  /**
     Gets the name of the state to which this city belongs
     @return The stateX name
  */
  public String getState() {}
}

/**
   An example of a non-political region implementing the Topology interface
*/
public class Desert extends Region implements TopographicalRegion {}
