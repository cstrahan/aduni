public class Rectangle extends Shape
{
    // Construct a unit circle
    public Rectangle()
    {
	this.corner1   = new Point(0.0,0.0);
	this.corner2   = new Point(0.0,1.0);
    }

    // Construct a circle from coords
    public Rectangle(double x1, double y1, double x2, double y2)
    {
	this.corner1  = new Point(x1, y1);
	this.corner2   = new Point(x2, y2);
    }
    
    // Construct a circle from two points
    public Rectangle(Point point1, Point point2)
    {
	this.corner1 = new Point(point1);
	this.corner2   = new Point(point2);
    }
    
    // show
    public String show()
    {
	String str = "Corner1 = " + corner1 + "    Corner2 = " + corner2;
	return str;
    }

    // Show object's type
    public String toString()
    {
	return "This is a Rectangle";
    }

    private Point corner1;
    private Point corner2;
}
