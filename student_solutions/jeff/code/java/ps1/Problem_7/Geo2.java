/**
   Geo2.java
   A second attempt at a geographical heirarchy.
   The organization for this model is very loose, and
   results in a great deal of replicated data (for example,
   every City has a separate hashtable for distances.)
   What would probably work much better would be to create
   Table objects which would keep track of various data,
   and give the objects methods to access the imformation
   they need.

   @author Jeffrey Radcliffe
   @version 0.2
*/

/**
   MapItem.java
*/
public abstract class MapItem
{
  /**
     Retrieves the objects id number
     @return int id number
  */
  public int getId() {}
  /**
     Sets the id number of the object
     (called only during constructor phase
  */
  public void setId() {}
  
  // fields
  private int id;
}

/**
   Country.java
*/
public class Country extends MapItem
{
  /**
     Constructor will set the id and its own fields
     @param name The name of the country
     @param capitol capitol of the country
     @param area the area of the country
  */
  Country(String name, String capitol, double area) {}
    
  /**
     Gets the cities of the country
     @return a String array of the cities
  */
  public String[] getCities() {}
  /**
     Gets the area of the country
     @return double area
  */
  public double area() {}
  /**
     Gets the capitol
     @return String capitol
  */
  public String capitol() {}
  /**
     Gets a list of the country's neighbors
     @return String array listing all the neighbors
  */
  public String[] neighbors() {}
  /**
     Checks to see if the country is a neighor
     @param country Country
     @return <code>true</code> if true, <code>false</code> otherwise
  */
  public boolean isNeighbor(Country country) {}
  /**
     Adds a neighbor to the country
     @param neighbor The neighbor to be added
     @return <code>true</code> if add successful, <code>false</code>
     otherswise
  */
  public boolean addNeighbor(String neighbor) {}
  /**
     Sets the capitol name.
     @param name Name of the new capitol
   */
  public void setCapitol(String name) {}
  public String toString() {}
  
  // fields
  private double area;
  private String capitol;
  private String[] neighbors;     // an array of neighboring countries
}

/**
   State.java
*/
public class State extends Country
{
  /**
     Constructor will set the name of the capitol and pass on the rest
     of the construction to the parent(Country)
  */
  State(String name, String capitol, String country, double area) {}

  /**
     Gets the current country
     @return The country to which this state belongs
  */
  public String getCountry() {}

  /**
     Gets the current state
     @return The state to which this state belongs
  */
  public String getState() {}

  /**
     Gets the current capitol 
     @return The capitol of the state
  */
  public String getCapitol() {}

  /**
     Sets the new country
     @param country The new country for the state
  */
  public void setCountry(String country) {}   // In case of war?

  /**
     Checks to see if the country is a neighor
     @param state another State
     @return <code>true</code> if true, <code>false</code> otherwise
  */
  public boolean isNeighbor(State state) {}

  /**
     Sets the new capitol
     @param capitol the new capitol
  */
  public void setCapitol(String capitol) {} // In case of disaster!

  /**
     Displays information about the state
     @return String information
  */
  public String toString() {}
  
  // fields
  private String myCountry;     // the country to which this state belongs
}

/**
   City.java
*/
public class City extends State
{
  // constructors
  City(String name, String state, String country, String capitol, int area) {}
  
  // methods
  /**
     Gets the distance between the current city and another
     @param aCity the city
     @param distance Distance from <code>aCity</code> to the current city
     @return The distance of the city, or -1 if <code>aCity</code> is not found.
  */
	public double distance(City aCity, double distance) {}
  /**
     Gets the state
     @return The state
  */
  public String getState() {}
  /**
     Gets a representation of the state
     @return some sort of representation
  */
  public String toString() {}

  // fields
  // a hashtable with citynames as keys and distances as values
  private Hashtable distanceBetweenCities;
  private String state;         // name of the current state
}

/**
   BoundrySegment.java
   A boundry segment has no name
*/
public abstract class BoundrySegment extends Thing
{
  /**
     Adds the argument to the vector of borders
     @param country Country
  */
  public void addBorderingCountry(Country country) {}

  /**
     Adds the argument to the vector of borders
     @param country Country
  */
  public void addBorderingState(State state) {}

  /**
     Checks to see if this border is a border of a particular country
     @param country the country in question
     @return <code>true</code> if the Country is bordered, <code>false</code> otherwise.
  */
  public boolean borderOf(Country country) {}

  /**
     Checks to see if this border is a border of a particular state
     @param state the state in question
     @return <code>true</code> if the State is bordered, <code>false</code> otherwise.
  */
  public boolean borderOf(State state) {}

  /**
     Returns the length of the boundry
     @return Length of the boundry
  */
  public double boundryLength() {}

  /**
     @return String information
  */
  public String toString() {}
    
  // fields
  private String[] borderingCountries;
  private String[] borderingStates;
  private double length;        // boundry length
}

/**
   NamedBoundrySegment.java
   This sort of boundry has a name!
*/
public abstract class NamedBoundrySegment extends BoundrySegment
{
  // fields
  private String name;
}

/**
   Here are some ideas for extensions of the boundry concept...
*/

// has no name, but can move often
public class PoliticalBorder extends BoundrySegment {}

// has a name, and can add extra fields such as pollution index
public class River extends NamedBoundrySegment {}

// has a name, and can add things such as a field of peaks with elevations,
// and so forth
public class MountainRange extends NamedBoundrySegment {}
