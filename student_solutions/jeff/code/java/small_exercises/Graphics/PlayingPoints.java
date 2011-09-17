import java.awt.*;

public class PlayingPoints
{
    public static void main(String[] args)
    {
	Point aPoint = new Point();
	Point bPoint = new Point(50,25);
	Point cPoint = new Point(bPoint);
	System.out.println("aPoint is located at: " + aPoint);

	aPoint.move(100,50);

	bPoint.x = 110;
	bPoint.y = 70;

	aPoint.translate(10,20);
	System.out.println("aPoint is now at: " + aPoint);

	if(aPoint.equals(bPoint))
	    System.out.println("aPoint and bPoint are at the same location.");
    }
}
	

	    
