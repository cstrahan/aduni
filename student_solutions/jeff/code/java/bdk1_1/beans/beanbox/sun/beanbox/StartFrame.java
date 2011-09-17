
/**
 * Show a welcome message while BeanBox starts.
 */

package sun.beanbox;

import java.awt.*;

public class StartFrame extends Frame {
    private static int width = 250;

    public StartFrame() {
	super("BeanBox");
	setBackground(Color.lightGray);

	Label l = new Label("Loading and analyzing jars...", Label.CENTER);
	add(l);
	l.setBounds(0, 35, width, 20);

	setBounds(200, 200, width, 70);
	show();
    }
}
