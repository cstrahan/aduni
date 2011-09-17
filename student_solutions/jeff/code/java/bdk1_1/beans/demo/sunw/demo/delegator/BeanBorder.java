
/*
 * This bean composes a bean and a ScrollPane.
 */

package sunw.demo.delegator;

import java.awt.*;
import java.beans.*;
import java.io.*;
import sunw.demo.classfile.*;
import java.applet.*;

public class BeanBorder extends Container {

    private Component bean;
    private Color borderColor = Color.red;
    private transient boolean started;
    private int borderWidth = 3;

    public BeanBorder() {
	setLayout(null);
    }

    public Dimension getPreferredSize() {
	if (bean == null) {
	    return new Dimension(20,20);
	}
	Dimension d = bean.getPreferredSize();
	return new Dimension(d.width + (2*borderWidth), d.height + (2*borderWidth));
    }

    public void slurp(Component b) {
	System.err.println("BeanBorder.slurp");
	bean = b;
	add(bean);
	if (bean instanceof Applet) {
	    ((Applet) bean).start();
	}
	doLayout();
    }

    public void doLayout() {
	if (!started && bean instanceof Applet) {
	    Applet applet = (Applet)bean;
	    applet.start();
	    started = true;
	}
	Dimension d = getSize();
	if (bean != null) {
	    bean.setBounds(borderWidth, borderWidth, 
		d.width-(2*borderWidth), d.height-(2*borderWidth));
	}
	repaint();
    }

    public void paint(Graphics g) {
	Dimension d = getSize();
	g.setColor(borderColor);
	g.fillRect(0, 0, d.width, borderWidth);
	g.fillRect(0, 0, borderWidth, d.height);
	g.fillRect(d.width-borderWidth, 0, borderWidth, d.height);
	g.fillRect(0, d.height-borderWidth, d.width, borderWidth);
    }

    public int getBorderWidth() {
        return borderWidth;
    }

    public void setBorderWidth(int b) {
	borderWidth = b;
	doLayout();
    }

    public Color getBorderColor() {
        return borderColor;
    }

    public void setBorderColor(Color c) {
	borderColor = c;
    }
}

