public class Country
{
    /**  Problem 7, Problem Set 1, Java/OOP.  Define a class structure for a program using
     *   countries, states, cities, boundaries and rivers.
     *   I know inner classes would work here, but I'm too lazy to do it.
     *   @author Tom Hickerson, January 2001.
     *   @param area The area of the country.
     *   @param capital The capitol of the country.
     *   @param boundary The length of the boundary of the country.
     */
    double area;
    String capital;
    double boundary;

    public Country(double a, String c, double b)
    {
	area = a;
	capital = c;
	boundary = b;
    }

    public double area()
    {
	return area;
    }

    public String capital()
    {
	return capital;
    }

    public double boundaryLength()
    {
	return boundary;
    }

    public Country getCountry()
    {
	return Country(area,capital,boundary);
    }
}    

public class State
{	/** Defines a class for states and their characteristics.
         *  @param name Name of the state.
         *  @param stateArea Area of the state.
         *  @param stateCapital Capitol of the state.
	 */
    String name;
    double stateArea;
    String stateCapital;

    public State(String n, double s, String c)
    {
	    name = n;
	    stateArea = s;
	    stateCapital =c;
    }

    public double area()
    {
	return stateArea;
    }

    public String capital()
    {
	return stateCapital;
    }

    public State getState()
    {
	return State(name,stateArea,stateCapital);
    }
}

public class Cities
{/** Defines a class for cities.
 *   @param name City name.
 *   @param cityArea Area of the city.
 *   @param isACapital Tells if the city is the state capitol or not.
 */
        String name;
	double cityArea;
	boolean isACapital;

	public Cities(String n, double a, boolean i)
	{
	    name = n;
	    cityArea = a;
	    isACapital = i;
	}

    public double area()
    {
	return cityArea;
    }
    
    public String getCity()
    {
	return name;
    }

    public boolean isCapital()
    {
	return isACapital;
    }

}

public class Boundaries
{/** A class defining boundaries.
 *   @param length The length of the boundary segment.
 *   @param neighbors The neighbors of the boundary segment.
 */
	double length;
	String neighbors;

	public Boundaries(double l, String n)
	{
	    length = l;
	    neighbors = n;
	}

    public double bLength()
    {
	return length;
    }
    
    public String neighbors()
    {
	return neighbors;
    }
}

public class Rivers
{/** A class defining rivers.
 *   @param length Length of the river.
 *   @param leftNeighbor One of the neighbors of the river.
 *   @param rightNeighbor One of the neighbors of the river.
 *   @param isABorder Whether or not the river is also a border.
 */
	double length;
	String leftNeighbor, rightNeighbor;
	boolean isABorder;

	public Rivers(double ln, String l, r, boolean i)
	{
	    length = ln;
	    leftNeighbor = l;
	    rightNeighbor = r;
	    isABorder = i;
	}

    public double rLength()
    {
	return length;
    }

    public String leftN()
    {
	return leftNeighbor;
    }
    
    public String rightN()
    {
	return rightNeighbor;
    }

    public boolean isBorder()
    {
	return isABorder;
    }
}

