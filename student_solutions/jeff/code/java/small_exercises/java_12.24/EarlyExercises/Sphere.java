public class Sphere
{
    static final double PI = 3.14;
    static int count = 0;

    // Instance variables
    double radius;

    double xCenter;
    double yCenter;
    double zCenter;

    // Class constructor
    Sphere(double theRadius, double x, double y, double z)
    {
	radius = theRadius;

	// Set the coordinates of the center
	xCenter = x;
	yCenter = y;
	zCenter = z;
	++count;                // upgrade object count
    }
    // Construct a unit sphere at a point
    Sphere(double x, double y, double z)
    {
	xCenter = x;
	yCenter = y;
	zCenter = z;
	radius = 1.0;
	++count;
    }

    // Another constructor,
    // creating a unit sphere at the origin
    Sphere()
    {
	xCenter = 0.0;
	yCenter = 0.0;
	zCenter = 0.0;
	radius = 1.0;
	++count;
    }
		  
   
    // Static method to get number of objects
    static int getCount()
    {
	return count;
    }

    // Instance method to calc volume
    double volume()
    {
	return 4.0/3.0 * PI * radius * radius * radius;
    }
}
