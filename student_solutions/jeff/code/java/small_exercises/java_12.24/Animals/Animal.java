public abstract class Animal implements Cloneable
{
    public Animal(String aType)
    {
	type = new String(aType);
    }
    
    public abstract void sound();
    
    public String toString()
    {
	return "This is a " + type;
    }

    private String type;
}
