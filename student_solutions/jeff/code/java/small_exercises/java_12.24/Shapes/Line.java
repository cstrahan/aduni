public class Line extends Shape
{
    // Construct a hypothetical line
    public Line()
    {
	this.start = new Point(0.0,0.0);
	this.end   = new Point(0.0,0.0);
    }

    // Construct a line from coords
    public Line(double x1, double y1, double x2, double y2)
    {
	this.start = new Point(x1, y1);
	this.end   = new Point(x2, y2);
    }
    
    // Construct a line from two points
    public Line(Point point1, Point point2)
    {
	this.start = new Point(point1);
	this.end   = new Point(point2);
    }
    
    // show
    public String show()
    {
	String str = "Start point = " + start + "    End point = " + end;
	return str;
    }

    // Show object's type
    public String toString()
    {
	return "This is a Line";
    }

    private Point start;
    private Point end;
}
