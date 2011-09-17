
package sun.beanbox;

import java.beans.*;
import java.lang.reflect.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import java.util.Vector;

public class PropertyNameDialog extends Dialog implements ActionListener {

    /*
     * Pop up a property dialog to look for a property of the given
     * source object.  If "match" is non-null, then "match" must be
     * equal to or a sunclass of the result property.
     * If "readable" is true, then readable bound properties will be
     * displayed, otherwise writable properties will be displayed.
     */

    PropertyNameDialog(Frame frame, Object source, String message, Class match, boolean readable) {
	super(frame, "PropertyNameDialog", true);
	new WindowCloser(this);
	setLayout(null);

	PropertyDescriptor allProperties[];
	try {
	    allProperties = Introspector.getBeanInfo(source.getClass()).getPropertyDescriptors();
	    if (allProperties == null) {
	        new ErrorDialog(frame, "PropertyNameDialog : couldn't find PropertyDescriptors");
		return;
	    }
	} catch (Exception ex) {
	    new ErrorDialog(frame, "PropertyNameDialog: Unexpected exception: \n" + ex);
	    return;
	}


	// Select matching properties
	properties = new Vector();
	for (int i = 0; i < allProperties.length; i++) {
	    PropertyDescriptor pd = allProperties[i];
	    if (readable && !pd.isBound()) {
		continue;
	    }
	    if (readable && pd.getReadMethod() == null) {
		continue;
	    }
	    if (!readable && pd.getWriteMethod() == null) {
		continue;
	    }
	    if (match == null || isSubclass(match, pd.getPropertyType())) {
		properties.addElement(pd);
	    }
	}

	int width = 300;

	if (properties.isEmpty()) {
	    new ErrorDialog(frame, "No suitable properties");
	    return;
	}

	int height = 200;

	Label l = new Label(message, Label.CENTER);
	l.setBounds(2, 30, width-4, 25);
	add(l);

	list = new List(8, false);
	for (int i = 0; i < properties.size(); i++) {
	    PropertyDescriptor pd = 
			(PropertyDescriptor)properties.elementAt(i);
	    list.add(pd.getDisplayName());
	}
	list.select(0);
	list.setBounds(10, 60, width-20, height-60);
	add(list);

	// Now do the "Cancel" and "OK" buttons.
	height += 10;
	cancelButton = new Button("Cancel");
	cancelButton.addActionListener(this);
	add(cancelButton);
	cancelButton.setBounds((width/2)-70, height-5, 60, 30);

	okButton = new Button("OK");
	okButton.addActionListener(this);
	add(okButton);
	okButton.setBounds((width/2)+10, height-5, 60, 30);
	height += 55;

	list.setBounds(10, 60, width-20, height-130);

	int x = frame.getLocation().x + 30;
	int y = frame.getLocation().y + 50;
	setBounds(x, y, width, height);
	show();
    }

    /**
     * Return true if class a is either equivalent to class b, or
     * if class a is a subclass of class b.
     * Note tht either or both "Class" objects may represent interfaces.
     */
    static  boolean isSubclass(Class a, Class b) {
	// We rely on the fact that for any given java class or
        // primtitive type there is a unqiue Class object, so
	// we can use object equivalence in the comparisons.
	if (a == b) {
	    return true;
	}
	if (a == null || b == null) {
	    return false;
	}
	for (Class x = a; x != null; x = x.getSuperclass()) {
	    if (x == b) {	
		return true;
	    }
	    if (b.isInterface()) {
		Class interfaces[] = x.getInterfaces();
		for (int i = 0; i < interfaces.length; i++) {
		    if (interfaces[i] == b) {
			return true;
		    }
		}
	    }
	}
	return false;
    }

    public PropertyDescriptor getResult() {
	return result;
    }

    public void actionPerformed(ActionEvent evt) {
	if (evt.getSource() == okButton) {
	    if (list != null) {
		int index = list.getSelectedIndex();
		if (index >= 0) {
		    result = (PropertyDescriptor)properties.elementAt(index);
		}
	    }
	}
	// For either canel or OK we're done.
	dispose();
    }


    private Button okButton;
    private Button cancelButton;
    private List list;

    private Vector properties;
    private PropertyDescriptor result;
}


