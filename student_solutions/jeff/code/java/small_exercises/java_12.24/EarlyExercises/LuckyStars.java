public class LuckyStars
{
    public static void main(String[] args)
    {
	String[] stars = {
	    "Robert Redford" , "Marilyn Monroe" ,
	    "Boris Karloff" ,  "Lassie" ,
	    "Hopalong Cassidy" , "Trigger",
	    "Elvis Presley" , "Keanu Reeves"
	};
	System.out.println("Your lucky star today is " 
 			   + stars[(int)(stars.length*Math.random())]
			   + ".");
    }
}
