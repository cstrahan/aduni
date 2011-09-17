
package sunw.demo.misc;

// TickTock is an invisible bean that simply fires a property
// change event at some regular specified interval.

import java.beans.*;
import java.util.Date;

public class TickTock implements Runnable, java.io.Serializable {

    public TickTock() {
	reset();
    }

    private void reset() {
	runner = new Thread(this);
	runner.start();
    }

    public int getSeconds() {
	return (int)((new Date()).getTime()/1000);
    }

    public int getInterval() {
	return interval;
    }

    public void setInterval(int seconds) {
	interval = seconds;
	if (runner != null) {
	    runner.interrupt();
	}
    }

    public void run() {
 	int oldSeconds = getSeconds();
	for (;;) {
	    try {
		Thread.sleep(interval * 1000);
	    } catch (InterruptedException ex) {
		// Just drop through.
	    }
	    int newSeconds = getSeconds();
	    changes.firePropertyChange("seconds", new Integer(oldSeconds),
						   new Integer(newSeconds));
	    oldSeconds = newSeconds;
	}
    }
    
    //----------------------------------------------------------------------
    // Methods for registering listeners:

    public void addPropertyChangeListener(PropertyChangeListener l) {
	changes.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
	changes.removePropertyChangeListener(l);
    }

    //----------------------------------------------------------------------
    // Support for serialization

    private void readObject(java.io.ObjectInputStream s)
        		throws java.lang.ClassNotFoundException,
			       java.io.IOException {
	s.defaultReadObject();
	// Compensate for missing constructor.
	reset();
    }

    private PropertyChangeSupport changes = new PropertyChangeSupport(this);
    private int interval = 5;
    transient Thread runner;
}
