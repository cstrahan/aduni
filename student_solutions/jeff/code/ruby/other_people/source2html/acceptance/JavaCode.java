// This is a comment

import java.util.Vector;

public class JavaCode
{
    public JavaCode () {
	itsStack = new Stack();
    }

    public synchronized void process () {
	while (itsStack.depth() > 0)
	    System.out.println ("Number = " + itsStack.top());
	    itsStack.pop();
	}
    }

    private Stack itsStack;
}
