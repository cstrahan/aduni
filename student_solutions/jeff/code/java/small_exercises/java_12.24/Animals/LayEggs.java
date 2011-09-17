public class LayEggs
{
    public static void main(String[] args)
    {
	Duck aDuck = new Duck("Donald", "Mallard");
	Animal aPet = aDuck; // Cast the Duck to Animal
	System.out.println(aPet);
	((Duck)aPet).layEgg();
	aPet.sound();
    }
}
