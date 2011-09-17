public class Map
{
  // constructors
  Map()
  {
    name = "default";
    
  }

  Map(String name)
  {
    this.name = name;
  }
  
  // methods
  public void setName(String name)
  {
    this.name = name;
  }
  public String getName()
  {
    return name;
  }
  public int getNumberOfItems()
  {
    return numberOfItems;
  }
  public String toString()
  {
    return ("Map: " + name);
  }
  
  // fields
  private String name;
  private int numberOfItems;
}
