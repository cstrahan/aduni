package Geometry;

public class Point
{
    // Create a point from coordinates
    public Point (double xVal, double yVal)
    {
	x = xVal;
	y = yVal;
    }

    // Create a point from another point object
    public Point (final Point oldPoint)
    {
	x = oldPoint.x;
	y = oldPoint.y;
    }

    // Move a point
    public void move(double xDelta, double yDelta)
    {
	x += xDelta;
	y += yDelta;
    }

    // Calculate the distance to another point
    public double distance(final Point aPoint)
    {
	return Math.sqrt(
			 (x - aPoint.x)*(x - aPoint.x) +
			 (y - aPoint.y)*(y - aPoint.y));
    }

    // Convert a point to a string
    public String toString()
    {
	return Double.toString(x) + ", " + y;
    }

    // Retrieve the x coordinate
    public double getX()
    { return x; }

    // Retrieve the y coordinate
    public double getY()
    { return y; }

    // Set the x coord
    public void setX(double inputX)
    { x = inputX; }

    // Set the y coord
    public void setY(double inputY)
    { y = inputY; }

    // coordinates of the point
    private double x;
    private double y;
}
