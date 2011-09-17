import java.util.Random;

public class TryPolymorphism
{
    public static void main(String[] args)
    {
	// Create an array of three different animals
	Animal[] theAnimals = {
	    new Dog("Rover", "Poodle"),
	    new Cat("Max" , "Tabby"),
	    new Duck("Daffy", "Aylesbury"),
	    new Spaniel("Fabian")
		};

	Animal petChoice;

	Random select = new Random();
	// Make five random choices of pet
	for(int i = 0; i < 3; i++)
	    {
		// Choose a random animal as a pet
		petChoice = theAnimals[select.nextInt(theAnimals.length)];

		System.out.println("\nYour choice:\n" + petChoice);
		petChoice.sound();
	    }
    }
}
