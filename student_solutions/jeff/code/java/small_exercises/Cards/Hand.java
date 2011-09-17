// Class defining a hand of cards
import java.util.*;

class Hand
{
    public void add(Card card)
    {
	hand.push(card);
    }

    public String toString()
    {
	Iterator cards = hand.iterator();

	StringBuffer str = new StringBuffer();
	while(cards.hasNext())
	    str.append(" " + (Card)cards.next());
	return str.toString();
    }

    private Stack hand = new Stack();
}
