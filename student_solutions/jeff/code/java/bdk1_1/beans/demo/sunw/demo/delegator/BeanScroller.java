
/*
 * This bean composes a bean and a ScrollPane.
 */

package sunw.demo.delegator;

import java.awt.*;
import java.beans.*;
import java.io.*;
import sunw.demo.classfile.*;
import java.applet.*;

public class BeanScroller extends ScrollPane {

    private Component bean;
    private transient boolean started;

    public BeanScroller() {
    }

    public void slurp(Component b) {
	System.err.println("BeanScroller.slurp");
	bean = b;
	add(bean);
	if (bean instanceof Applet) {
	    ((Applet) bean).start();
	}
    }

    public void doLayout() {
	super.doLayout();
	if (!started && bean instanceof Applet) {
	    Applet applet = (Applet)bean;
	    applet.start();
	    started = true;
	}
    }

    public void setPaneBackground(Color c) {
	if (bean == null) {
	    return;
	}
	bean.setBackground(c);
    }

    public Color getPaneBackground() {
	if (bean == null) {
	    return getBackground();
	}
	return bean.getBackground();
    }
}


