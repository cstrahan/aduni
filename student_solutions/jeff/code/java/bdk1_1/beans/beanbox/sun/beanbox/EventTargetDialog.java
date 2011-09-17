
package sun.beanbox;

import java.beans.*;
import java.lang.reflect.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import java.util.Vector;

public class EventTargetDialog extends Dialog implements Runnable, ActionListener {

    EventTargetDialog(Frame frame, Wrapper sourceWrapper, Wrapper targetWrapper,
			EventSetDescriptor esd, Method listenerMethod) {
	super(frame, "EventTargetDialog", false);
	new WindowCloser(this);
	setLayout(null);
	this.esd = esd;
	this.listenerMethod = listenerMethod;

	Vector matchMethods = new Vector();
	Vector zeroArgMethods = new Vector();

	this.sourceWrapper = sourceWrapper;
	this.targetWrapper = targetWrapper;
	Object target = targetWrapper.getBean();

	try {

	    // Search for target methods that either match the event
	    // listener signature or which have zero args and no results.

	    MethodDescriptor mds[] = Introspector.getBeanInfo(target.getClass()).getMethodDescriptors();
	    Class eargs[] = listenerMethod.getParameterTypes();
	    Class eExceptions[] = listenerMethod.getExceptionTypes();

	    for (int i = 0; i < mds.length; i++) {
		MethodDescriptor md = mds[i];
	        Class margs[] = md.getMethod().getParameterTypes();
	        Class mExceptions[] = md.getMethod().getExceptionTypes();
     
		if (margs.length == 0
		    && mExceptions.length == 0
		    && md.getMethod().getReturnType() == Void.TYPE) {
		    zeroArgMethods.addElement(md);
		    continue;
		}
			// the target method must have the same number of formal params
			// and have no more (a proper subset of) checked exceptions
			// in its throws clause
	        if ((eargs.length != margs.length)
	        	|| (eExceptions.length > mExceptions.length)) {
		    continue;
	        }
	        boolean match = true;
	        // check to see if formal parameter types agree
	        for (int j = 0; j < eargs.length; j++) {
		    if (!isSubclass(eargs[j], margs[j])) {
		        match = false;
		        break;
		    }
		    }
		    
		    // check to see if checked exception types agree
	        for (int j = 0; j < eExceptions.length; j++) {
		    if (!isSubclass(eExceptions[j], mExceptions[j])) {
		        match = false;
		        break;
		    }	    
	        }
		if (match) {
		    matchMethods.addElement(md);
		}
	    }
	} catch (Exception ex) {
	    new ErrorDialog(frame, "EventTargetDialog: Unexpected exception: \n" + ex);
	    return;
	}

	int width = 300;

	sortMethods(matchMethods);
	sortMethods(zeroArgMethods);
	int count = matchMethods.size() + zeroArgMethods.size();
	methods = new MethodDescriptor[count];

	for (int i = 0; i < matchMethods.size(); i++) {
	    methods[i] = (MethodDescriptor) matchMethods.elementAt(i);
	}
	for (int i = 0; i < zeroArgMethods.size(); i++) {
	    methods[i + matchMethods.size()] = (MethodDescriptor) zeroArgMethods.elementAt(i);
	}

	if (count == 0) {
	    new ErrorDialog(frame, "No suitable target method on\n" + 
					target.getClass().getName());
	    return;
	}

	int height = 200;

	Label l = new Label("Please chose a target method:", Label.CENTER);
	l.setBounds(2, 30, width-4, 25);
	add(l);

	list = new List(8, false);
	for (int i = 0; i < methods.length; i++) {
	    list.add(methods[i].getName());
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


    // Run is called in a sepaarte thread to actually complete a
    // requested event hookup.

    public void run() {
	int index = list.getSelectedIndex();

	// Remove the current dialog, and put up a status line.	
	removeAll();
	Label status = new Label("Generating and compiling adaptor class");
	add(status);
	status.setBounds(20, getSize().height/2, getSize().width-30, 25);
	repaint();

	HookupManager.hookup(esd, listenerMethod, sourceWrapper,
			targetWrapper, methods[index].getMethod());

	dispose();
    }

    public void actionPerformed(ActionEvent evt) {
	if (evt.getSource() == okButton) {
	    Thread th = new Thread(this);
	    th.start();
	} else if (evt.getSource() == cancelButton) {
	    dispose();
	}
    }

    /**
      * Do a simple bubble sort on a Vector of MethodDescriptors.
      */
    private void sortMethods(Vector methods) {
	for (int i = methods.size()-2; i >= 0; i--) {
	    for (int j = 0; j <= i; j++) {
		String s1 = ((MethodDescriptor)methods.elementAt(j)).getName();
		String s2 = ((MethodDescriptor)methods.elementAt(j+1)).getName();
		if (s1.compareTo(s2) > 0) {
		   Object tmp = methods.elementAt(j);
		   methods.setElementAt(methods.elementAt(j+1), j);
		   methods.setElementAt(tmp, j+1);
		}
	    }
	}
    }

    /**
     * Return true if class a is either equivalent to class b, or
     * if class a is a subclass of class b.
     * Note tht either or both "Class" objects may represent interfaces.
     */
    static boolean isSubclass(Class a, Class b) {
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

    private Button okButton;
    private Button cancelButton;
    private List list;

    private EventSetDescriptor esd;
    private Method listenerMethod;
    private Wrapper sourceWrapper;
    private Wrapper targetWrapper;
    private MethodDescriptor methods[];
}
