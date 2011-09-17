
package sunw.demo.transitional;

/**
 * The OurButton class is a very minimal bean that simply extends the 
 * standard JDK 1.0.2 Button class to throw a bean event.
 */

import java.util.Vector;
import java.awt.*;

public class OurButton extends java.awt.Button implements
                                        sunw.io.Serializable {
    private boolean dbg;
    private Vector listeners = new Vector();

    public OurButton() {
        this("press");
    }

    public OurButton(String label) {
        super(label);
    }

    // Register an Event Listener.
    public synchronized void addButtonPushListener(ButtonPushListener bl) {
        listeners.addElement(bl);
    }

    // Remove an Event Listener.
    public synchronized void removeButtonPushListener(ButtonPushListener bl) {
        listeners.removeElement(bl);
    }

    // Catch an old style AWT event and fire it as a new style event.

    public boolean handleEvent(Event evt) {
        if (evt.id == Event.ACTION_EVENT) {

            // Notify each our our listeners.
            Vector        l;
            ButtonPushEvent         e = new ButtonPushEvent(this);
            synchronized(this) {
                l = (Vector) listeners.clone();
            }

            for (int i = 0; i < l.size(); i++) {
                ButtonPushListener bl =
                            (ButtonPushListener)l.elementAt(i);
                bl.push(e);
            }

        }
        return super.handleEvent(evt);
   }

    public boolean isDebug() {
	return dbg;
    }

    public void setDebug(boolean x) {
        dbg = x;
    }
}

