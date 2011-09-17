import java.util.*;

public class JekyllAndHyde extends Observable
{
    String name = "Dr. Jekyll";

    public void drinkPotion()
    {
	name = "Mr. Hyde";
	setChanged();
	notifyObservers();
    }

    public String getName()
    {
	return name;
    }

}
