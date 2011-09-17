class Countries
{/** Problem Set one, number 7, Class Design:
 *   Using a private four-dimensional array of Strings, we can construct a class system
 *   much like the one asked of us in the PSet, the first field would govern countries,
 *   the second information fields, the third states/provinces, the fourth cities.
 *   @author Tom Hickerson, January 2001.
 */
    String areaData[] [] [] []= new String[MAXCOUNTRIES] [MAXFIELDS] 
	[MAXSTATES] [MAXCITIES];

    public static area(int name)
    {
	return areaData[name] [1] [0] [0];
    }

    public static capital(int name)
    {
	return areaData[name] [2] [0] [0];
    }

    public static boundaryLength(int name)
    {
	return areaData[name] [3] [0] [0];
    }

    public static getCountry(int name)
    {
	return areaData[name] [0] [0] [0];
    }
}

class StatesProvinces extends Countries
{
    public static stateArea(int name, int state)
    {
	return areaData[name] [5] [state] [0];
    }

    public static stateCapital(int name, int state)
    {
	return areaData[name] [6] [state] [0];
    }

    public static stateNeighbors(int name, int state)
    {
	return areaData[name] [7] [state] [0];
    }

    public static getState(int name, int state)
    {
	return areaData[name] [4] [state] [0];
    }
}

class Segments extends Countries
{
    public distanceOf(int name, int state)
    {
	return areaData[name] [12] [state] [0];
    }

    public borderOf(int name, int state)
    {
	return areaData[name] [13] [state] [0];
    }
}

class Rivers extends StatesProvinces
{
    public borderOf(int name, state)
    {
	return areaData[name] [10] [state] [0];
    }

    public boundaryLength(int name, state)
    {
	return areaData[name] [11] [state] [0];
    }
}

class Cities extends StatesProvinces
{
    public cityName(int name, state, city)
    {
	return areaData[name] [4] [state] [city];
    }
    
    public cityArea(int name, state, city)
    {
	return areaData[name] [5] [state] [city];
    }

    public cityEconomy(int name, state, city)
    {
	return areaData[name] [6] [state] [city];
    }

    public distance(int name, state, city)
    {
	return areaData[name] [8] [state] [city];
    }
}
