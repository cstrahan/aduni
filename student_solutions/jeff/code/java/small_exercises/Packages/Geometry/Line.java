package Geometry;

public class Line
{
    // Create a ling from two points
    public Line(final Point start, final Point end)
    {
	this.start = new Point(start);
	this.end = new Point(end);
    }

    // Create a line from two coordinate pairs
    public Line(double xStart, double yStart, double xEnd, double yEnd)
    {
	start = new Point (xStart, yStart);
	end = new Point (xEnd, yEnd);
    }

    // Calculate the length of a line
    public double length()
    {
	return start.distance(end);
    }

    // Return a point as the intersection of two lines --
    // called from a Line object
    public Point intersects(final Line line1)
    {
	Point localPoint = new Point(0, 0);

	double num =
	    (this.end.getY() - this.start.getY()) * (this.start.getX() - line1.start.getX()) -
	    (this.end.getX() - this.start.getX()) * (this.start.getY() - line1.start.getY());

	double denom =
	    (this.end.getY() - this.start.getY()) * (line1.end.getX() - line1.start.getX()) -
	    (this.end.getX() - this.start.getX()) * (line1.end.getY() - line1.start.getY());

	localPoint.setX(line1.start.getX() + (line1.end.getX() - line1.start.getX())*num/denom);
	localPoint.setY(line1.start.getY() + (line1.end.getY() - line1.start.getY())*num/denom);

	return localPoint;
    }

    // Convert a line to a string
    public String toString()
    {
	return "(" + start+ "):(" + end + ")";
    }

    // Data members
    Point start;
    Point end;


}
