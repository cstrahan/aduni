public class Country extends MapItem
{
  // constructors
  Country()
  {
    super();
    numberOfCountries++;
  }
  Country(String name)
  {
    super(name);
    numberOfCountries++;
  }
  Country(String name, String capitol)
  {
    this(name);
    this.capitol = capitol;
  }
  Country(String name, String capitol, double area)
  {
    this(name, capitol);
    this.area = area;
  }
    
  // methods
  //  public String getCities() {}
  public double area() { return area; }
  public String capitol() { return capitol; }
  public void neighbors() {}
  public void addNeighbor() {}
  public void setCapitol(String capitol) { this.capitol = capitol; }
  public void addState(String name)
  {
    
  public void addBoundry() {}
  public void addRiver() {}                 // assuming a country can own a river
  public String toString()
  {
    return ("The Country of " + getName());
  }
  
  // fields
  private double area = 0.0;
  private String capitol = "";
  private static int numberOfCountries;
}
