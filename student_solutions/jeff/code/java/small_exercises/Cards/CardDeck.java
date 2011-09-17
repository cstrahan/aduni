// A Deck of Cards!
import java.util.*;

class CardDeck
{
    // Create a deck of 52 cards
    public CardDeck()
    {
	for(int theSuit = Card.HEARTS; theSuit <= Card.SPADES; theSuit++)
	    for(int theValue = Card.ACE; theValue <= Card.KING; theValue++)
		deck.push(new Card(theValue, theSuit));
    }

    // Deal a hand
    public Hand dealHand(int numCards)
    {
	Hand hand = new Hand();
	for(int i = 0; i < numCards; i++)
	    hand.add((Card)deck.pop());
	return hand;
    }

    // Shuffle the deck
    public void shuffle()
    {
	Collections.shuffle(deck);
    }

    private Stack deck = new Stack();
}
	    
		    
    
