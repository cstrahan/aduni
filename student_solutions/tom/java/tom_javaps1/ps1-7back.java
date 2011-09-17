public class Country
{
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
{	    
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
{
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
{
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
{
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

