
package sunw.demo.transitional;

/**
 * A very simple demo applet that uses the "OurButton" bean
 * and the "TransitionalBean" bean.
 *
 * We ctach "ButtonPush" events from the button and then change the
 * color on the target TransitionalBean.
 */

import java.awt.Color;

public class Flipper extends java.applet.Applet implements ButtonPushListener {

    public void init() {

	// We do our own hand layout.
	setLayout(null);

	// Create a TransitionalBean.
	jb = new TransitionalBean();
	jb.setColor(firstColor);
	add(jb);
	jb.reshape(250, 25, 100, 40);

	// Create a button.
	btn = new OurButton();
	btn.setLabel("flip");
	add(btn);
	btn.reshape(100, 30, 100, 30);

	// Add ourself as an event handler object for button pushes.
	btn.addButtonPushListener(this);
    }

    /**
     * This methods catches a button push event and uses it to
     * flip the color off our TransitionalBean between two choices.
     */

    public void push(ButtonPushEvent evt) {
	if (jb.getColor() == firstColor) {
	    jb.setColor(secondColor);
	} else {
	    jb.setColor(firstColor);
	}
    }

    private Color firstColor = Color.green;
    private Color secondColor = Color.blue;
    private TransitionalBean jb;
    private OurButton btn;
}
