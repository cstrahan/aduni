public abstract class Shape
{
    // Moves a shape
    public void moveShape(double xDelta, double yDelta)
    {
	x += xDelta;
	y += yDelta;
    }

    public double getX()
    { return x; }

    public double getY()
    { return y; }

    // Other Methods
    public abstract String show(); // doesn't do anything

    // Data Members
    protected double x;
    protected double y;
}
