class TryDeal
{
    public static void main(String[] args)
    {
	CardDeck deck = new CardDeck();
	deck.shuffle();

	Hand myHand = deck.dealHand(5);
	Hand yourHand = deck.dealHand(5);
	System.out.println("\nMy hand is " + myHand);
	System.out.println("\nYour hand is " + yourHand);
    }
}
