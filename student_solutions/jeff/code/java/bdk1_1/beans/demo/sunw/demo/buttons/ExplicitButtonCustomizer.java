
package sunw.demo.buttons;

import java.awt.*;
import java.awt.event.*;
import java.beans.*;

public class ExplicitButtonCustomizer extends Panel implements Customizer, KeyListener {

    public ExplicitButtonCustomizer() {
	setLayout(null);
    }

    public void setObject(Object obj) {
	target = (ExplicitButton) obj;

	Label t1 = new Label("Caption:", Label.RIGHT);
	add(t1);
	t1.setBounds(10, 5, 60, 30);

	labelField = new TextField(target.getLabel(), 20);
	add(labelField);
	labelField.setBounds(80, 5, 100, 30);

	labelField.addKeyListener(this);
    }

    public Dimension getPreferredSize() {
	return new Dimension(200, 40);
    }

    /**
     * @deprecated provided for backward compatibility with old layout managers.
     */
    public Dimension preferredSize() {
	return getPreferredSize();
    }

    public void keyTyped(KeyEvent e) {
    }

    public void keyPressed(KeyEvent e) {
    }

    public void keyReleased(KeyEvent e) {
	String txt = labelField.getText();
	target.setLabel(txt);
	support.firePropertyChange("", null, null);
    }

    //----------------------------------------------------------------------

    public void addPropertyChangeListener(PropertyChangeListener l) {
	support.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
	support.removePropertyChangeListener(l);
    }

    private PropertyChangeSupport support = new PropertyChangeSupport(this);

    //----------------------------------------------------------------------

    private ExplicitButton target;
    private TextField labelField;
}
