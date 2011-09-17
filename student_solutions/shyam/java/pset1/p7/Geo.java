/**
   This is an outline of a Geographical model.
   
   @author Shyam Visweswaran
*/

/**
   Superclass that creates 1-dimensional geographical objects called
   GeoPoints. Each GeoPoint has a X and a Y coordinate.
*/
public class GeoPoint 
{
  // instance variables
  private double xCoord = 0.0;
  private double yCoord = 0.0;
  
  // contructors go here

  // methods
  /**
     Get the X coordinate of the GeoPoint.
  */
  public double getX() { }

  /**
     Get the Y coordinate of the GeoPoint.
  */
  public double getY() { }
}

/** Superclass that computes the length of geographical objects.
    It is abstract to prevent instantiation. This could also be
    an interface.
*/
public abstract class GeoDistance
{
  /**
     Compute the length between 2 points.
  */
  public double lengthOf(GeoPoint a, GeoPoint b) { }
}

/**
   Superclass that creates 2-dimensional geographical objects called
   GeoLines. Each GeoLine is defined by 2 GeoPoints that are the end-points
   of the line. The length of a GeoLine is a derived property that is computed
   from the 2 GeoPoints.
*/
public class GeoLine extends GeoDistance
{
  // instance variables
  private Point startPoint;
  private Point endPoint;

  // constructors go here

  // methods
  /**
     Get the length of the GeoLine.
  */
  public double getLength() { }
}

/**
   Superclass that creates 3-dimensional geographical objects called
   GeoRegions. Each GeoRegion has a center, area, and two arrays for
   its neighbors and boundaries. Perimeter is computed from the lengths
   of all the boundaries.
*/
public class GeoRegion extends GeoDistance
{
  // instance variables
  private GeoPoint center; // the center of a region
  private String name; // name of the region
  private double area; // area of a region
  private String[] neighbors; // a string array that stores neighbors
  private String[] boundaries; // a string array that stores boundaries
  private long population; // population of the region

  // constructors go here

  // methods
  /**
     Get the name of the GeoRegion.
  */
  public String getName() { }

  /**
     Get the center of the GeoRegion (as a GeoPoint).
  */
  public GeoPoint getCenter() { }

  /**
     Get the area of the GeoRegion.
  */
  public double getArea() { }

  /**
     Compute the perimeter of the GeoRegion from the lengths of the boundaries.
  */
  public double getPerimeter() { }

  /**
     Get the neighbors of the GeoRegion.
  */
  public String getNeighbors() { }
  
  /**
     Get the boundaries of the GeoRegion.
  */
  public String getBoundaries() { }
 
  /**
     Get the population of the GeoRegion.
  */
  public long getPopulation() { }
}

/**
   Borders are GeoLines with a list of GeoRegions that they border.
*/
public class Border extends GeoLine
{
  // instance variables
  private String[] borderItems; // bordering regions

  // constructors go here

  // methods
  /**
     Get the bordering regions.
   */
  public String borderOf() { }
  
  /**
     Get the length of the border.
  */
  public double boundaryLength() { }
}

/**
   Rivers are GeoLines with a list of GeoRegions that they pass through.
*/
public class River extends GeoLine
{
  // instance variables
  private String riverName; // name of river
  private String[] riverItems; // regions through which the river passes

  // constructors go here

  // methods
  /**
     Get the regions through which the river passes.
  */
  public String passesThru() { }

  /**
     Get the length of the river from source to end.
  */
  public double riverLength() { }
}

/**
   Countries are GeoRegions that have a name, capital and list of states.
*/
public class Country extends GeoRegion
{
  // instance variables
  private String countryCapital; // name of capital of country
  private String[] countryStates; // list of states in the country

  // constructors

  // methods
  /**
     Get the capital of the country.
  */
  public String getCapital() { }

  /**
     Get the states of the country.
  */
  public String getStates() { }
}

/**
   States are GeoRegions that have a capital and the country where they are located.
   They also have a list of cities located in the state.
*/
public class State extends GeoRegion
{
  // instance variables
  private String country; // country in which the state is located
  private String stateCapital; // name of capital of state
  private String[] stateCities; // list of cities in the state

  // constructors

  // methods
  /**
     Get capital of the state.
  */
  public String getCapital() { }

  /**
     Get the cities located in the state.
  */
  public String getCities() { }

  /**
     Get the country in which the state is located.
  */
  public String getCountry() { }
}

/**
   Cities are GeoRegions that store the state and country in which
   they are located.
*/
public class City extends GeoRegion
{
  // instance variables
  private String country; // country in which the city is located
  private String state; // state in which the city is located
  
  // constructors

  // methods
  /**
     Get the country in which the city is located.
  */
  public String getCountry() { }

  /**
     Get the state in which the city is located.
  */
  public String getState() { }

  /**
     Get the distance between this city and the other city.
     @param otherCity the other city
  */
  public double getDistance(City otherCity) { }
}
