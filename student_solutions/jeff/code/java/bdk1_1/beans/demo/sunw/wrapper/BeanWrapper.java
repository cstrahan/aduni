
package sunw.wrapper;

/**
 * Some beans are themselves applets.  These beans can be run directly
 * as applets from within HTML documents.
 *
 * However, some beans are not applets.  This simple Warpper applet
 * allwos you run run an arbitrary named bean class inside a Wrapper on an
 * HTML page.  It takes a single parameter "BEAN" that gives a bean name
 * such as "sunw.demo.juggler.Juggler".
 */

import java.applet.Applet;
import java.awt.*;
import java.beans.Beans;

public class BeanWrapper extends Applet {
    private Component bean;
    private String error;

    public void init() {
	setLayout(null);
	String beanName = getParameter("BEAN");
	if (beanName == null) {
	    error = "no \"BEAN\" parameter defined";
	    System.err.println(error);
	    showStatus(error);
	    return;
	}
	try {
	    ClassLoader cl = this.getClass().getClassLoader();
	    Object o = Beans.instantiate(cl, beanName);
	    bean = (Component) o;
	} catch (Exception ex) {
	    error = "Couldn't instantiate bean " + ex;
	    System.err.println(error);
	    showStatus(error);
	}
    }

    public void start() {
	if (bean == null) {
	    repaint();
	    return;
	}
	removeAll();
	add(bean);
	bean.setSize(getSize());
	if (Beans.isInstanceOf(bean, Applet.class)) {
	    Applet apl = (Applet) Beans.getInstanceOf(bean, Applet.class);
	    apl.start();
	}
    }

    public void stop() {
	if (bean == null) {
	    return;
	}
	if (Beans.isInstanceOf(bean, Applet.class)) {
	    Applet apl = (Applet) Beans.getInstanceOf(bean, Applet.class);
	    apl.stop();
	}
    }

    public void destroy() {
	if (bean == null) {
	    return;
	}
	if (Beans.isInstanceOf(bean, Applet.class)) {
	    Applet apl = (Applet) Beans.getInstanceOf(bean, Applet.class);
	    apl.destroy();
	}
    }

    public void paint(Graphics g) {
	if (error != null) {
	    g.drawString(error, 10, 30);
	    return;
	}
    }

}
