class Card implements Comparable
{
    public Card(int value, int suit) throws IllegalArgumentException
    {
	if(value >= ACE && value <= KING)
	    this.value = value;
	else
	    throw new IllegalArgumentException("Invalid card value");
	if(suit >= HEARTS && suit <= SPADES)
	    this.suit = suit;
	else
	    throw new IllegalArgumentException("Invalid suit");
    }

    // Compare two cards
    public int compareTo(Object card)
    {
	if(suit != ((Card)card).suit)
	    return suit < ((Card)card).suit ? -1 : 1;
	else
	    if(value == ((Card)card).value)
		return 0;
	else
	    return value < ((Card)card).value ? -1 : 1;
    }

    public String toString()
    {
	String cardStr = "" + value;
	switch(value)
	    {
	    case ACE:
		cardStr = "A";
		break;
	    case JACK:
		cardStr = "J";
		break;
	    case QUEEN:
		cardStr = "Q";
		break;
	    case KING:
		cardStr = "K";
		break;
	    }

	switch(suit)
	    {
	    case CLUBS:
		cardStr += "C";
		break;
	    case DIAMONDS:
		cardStr += "D";
		break;
	    case HEARTS:
		cardStr += "H";
		break;
	    case SPADES:
		cardStr += "S";
		break;
	    }
	return cardStr;
    }

    // Suit values
    public static final int HEARTS = 0;
    public static final int CLUBS = 1;
    public static final int DIAMONDS = 2;
    public static final int SPADES = 3;

    // Card face values
    public static final int ACE = 1;
    public static final int JACK = 11;
    public static final int KING = 12;
    public static final int QUEEN = 13;

    private int suit;
    private int value;
}
