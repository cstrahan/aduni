public class MathCalc
{
    public static void main(String[] args)
    {
	// Calculate the radius of a circle
	// which has an area of 100 square feet
	double radius = 0.0;
	double circleArea = 100.0;
	int feet = 0;
	int inches = 0;
	radius = Math.sqrt(circleArea/Math.PI);
	feet = (int)Math.floor(radius);
	inches = (int) Math.round(12.0*(radius-feet));
	System.out.println("The radius of a circle with area " + circleArea
			   + " square feet is\n\t" + feet + " feet " +
			   inches + " inches");
    }
}
