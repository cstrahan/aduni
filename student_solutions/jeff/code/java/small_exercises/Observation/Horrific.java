// Try out observers
import java.util.*;

public class Horrific
{
    public static void main(String[] args)
    {
	JekyllAndHyde man = new JekyllAndHyde();  // Create the good Doctor

	Observer[] crowd = {
	    new Person("Officer" , "What's all this then?"),
	    new Person("Eileen Backwards", "Oh, the humanity!"),
	    new Person("Phil McCrackup" , "Gadzooks!"),
	    new Person("Elvis Presley" , "Oooh, can't stop dancing!"),
	    new Person("Jerry Garcia" , "Peace out, man.")};

	// Add the observers
	for(int i  =0 ; i < crowd.length; i++)
	    man.addObserver(crowd[i]);

	man.drinkPotion();
    }
}
