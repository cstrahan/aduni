public class Circle extends Shape
{
    // Construct a unit circle
    public Circle()
    {
	this.center   = new Point(0.0,0.0);
	this.radius   = new Point(0.0,1.0);
    }

    // Construct a circle from coords
    public Circle(double x1, double y1, double x2, double y2)
    {
	this.center  = new Point(x1, y1);
	this.radius   = new Point(x2, y2);
    }
    
    // Construct a circle from two points
    public Circle(Point point1, Point point2)
    {
	this.center = new Point(point1);
	this.radius   = new Point(point2);
    }
    
    // show
    public String show()
    {
	String str = "Center = " + center + "    Radius = " + radius;
	return str;
    }

    // Show object's type
    public String toString()
    {
	return "This is a Circle";
    }

    private Point center;
    private Point radius;
}
