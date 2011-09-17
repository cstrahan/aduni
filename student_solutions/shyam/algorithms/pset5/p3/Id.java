/*
  Algorithms: PSet 5 - Problem 3
  Shyam Visweswaran
*/
public class Id
{
  int i; // x coordinate
  int j; // y coordinate
  
  public Id(int i, int j)
  {
    this.i = i;
    this.j = j;
  }
  
  public int i()
  {
    return i;
  }
  
  public int j()
  {
    return j;
  }
  
  public String toString()
  {
    return (i + "-" + j);
  }
}
