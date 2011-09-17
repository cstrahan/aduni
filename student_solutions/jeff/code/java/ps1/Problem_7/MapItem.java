public abstract class MapItem
{
  // constructor
  MapItem()
  {
    this.name = "default";
    id = numberOfItems++;
  }
  
  MapItem(String name)
  {
    this.name = name;
    id = numberOfItems++;
  }
  
  // methods
  public int getId()
  {
    return id;
  }

  public String getName()
  {
    return name;
  }

  public void setName(String name)
  {
    this.name = name;
  }
  
  public String toString()
  {
    return ("Generic MapItem id #" + id + " - " + name);
  }
  
  // fields
  private String name;
  private int id;
  private static int numberOfItems;
}
