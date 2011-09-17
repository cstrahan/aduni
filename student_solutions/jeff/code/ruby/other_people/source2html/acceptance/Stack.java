package source2html.stuff;

public class Stack
{
    public Stack() {
	itsDepth = 0;
	itsValues = new int[maxDepth];
    }

    // Queries -------------------------------------------------------

    public int depth () {
	return itsDepth;
    }

    public int top () {
	return itsValues[itsDepth-1];
    }

    public boolean isEmpty () {
	return itsDepth == 0;
    }

    public boolean isFull () {
	return itsDepth == maxDepth;
    }

    // Commands ------------------------------------------------------

    public void push (int value) {
	itsValues[itsDepth] = value;
	itsDepth++;
    }

    public void pop () {
	itsDepth--;
    }

    // Implementation ------------------------------------------------

    private static final int maxDepth = 10;
    private int itsDepth;
    private int[] itsValues;
}
