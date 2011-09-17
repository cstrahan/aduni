import java.util.Random;           // Randomizer

public class Driver
{
    public static void main(String[] args)
    {
	Shape[] theShapes = {
	    new Line(),
	    new Circle(),
	    new Rectangle()
	};

	Shape shapeChoice;

	Random select = new Random();
	for(int i = 0 ; i < 5 ; i++)
	    {
		shapeChoice = theShapes[select.nextInt(theShapes.length)];

		System.out.println(shapeChoice);
		System.out.println(shapeChoice.show());
	    }
    }
}
