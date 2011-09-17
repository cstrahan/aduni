/* Problem 7, problem set 1, Java/OOP.  Class Design: this text file is a plan
 * to support the following classes: Country, State, City, River, and Boundary,
 * containing the following operations: area(), capital(), distance(), 
 * boundaryLength(), neighbors(), borderOf(), amoung others.
 */

public class Segment
{/** Line, a straight line which has lots of uses.
 *   
 */
    private double length;
    public Segment (double l);
    public double getLength();
    //Displays the distance between two points.
}

public class River extends Segment
{/** River, one of the first real classes.  Two arrays are defined
 *   here; one is the route of the river and the other is declaring
 *   whether any given section of the route is a border or not.
 *   @param route The path of the River.
 *   @param isABorder Declares where a section of the river is a border or not.
 */
    Segment[] route = new Segment[];
    boolean[] isABorder = new boolean[];
}

public class Boundary extends Segment
{/** Boundary, which extends Area because it is not necessarily a
 *   straight line.  One might consider it an Area where the 
 *   starting point does not equal its ending point.
 */
    public double boundaryLength();
    //Displays the length of the Boundary.
}

public class Region
{/** Region, which is the superclass for Cities, States, and Countries.
 */

    private double area;
    public double getArea();
    //Displays the area of that Region.

    private City capitol;
    public City getCapitol();
    //

    public Region[] neighbors();
    //Displays the Regions that are a neighbor to that region.
}

public class City extends Region
{/** City, an area of region.
 *   @param lng Longitude, coordinates for a city location.
 *   @param lat Latitude, coordinates for a city loacation.
 */
    private double lng;
    private double lat;
    public static double distance(City a, City b);
    //Computes the distance between two cities.
}

public class State extends Region
{/** State, also a subclass of Region.
 *   @param cities All the cities in the state.
 *   @param rivers All the rivers in the state.
 *   @param river A river cited in borderOf.
 *
 */
    City[] cities = new City[];
    River[] rivers = new River[];
    public City[] getCities();
    //Displays a list of cities in that State.
    public Region borderOf(River river)
    //Returns the Regions that a given river separates.
}

public class Country extends Region
{/** Country, an extension of region.
 *   @param states All the states in the country.
 */
    State[] states = new State[];
}

public class World
{/** World, an extension of countries.
 *   @param countries All the countries in the world.
 */
    Country[] countries = new Country[];
    public Country getCountry(String name, Country[] countries);
    //Fetches the Country in question that matches the name.
}
