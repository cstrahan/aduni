
package sunw.demo.misc;

import java.awt.*;
import java.beans.*;
import java.io.Serializable;


/**
 * A simple extension of TextField that handles PropertyChangeEvents.  
 * This Bean handles PropertyChange events by displaying the name of 
 * the property and the new property value.  A Java Bean that displays
 * bound properties can be connected to a ChangeReporter with the
 * BeanBox.  To do so programatically is straightforward:
 * 
 * <pre>
 * import sunw.demo.misc.OurButton;
 * import sunw.demo.misc.ChangeReporter;
 * import java.awt.*;
 * import java.beans.*;
 *   
 * public class DemoChangeReporter
 * {
 *   OurButton button = new OurButton(); 
 *   ChangeReporter reporter = new ChangeReporter();
 *   PropertyChangeAdapter adapter = new PropertyChangeAdapter();
 *     
 *   DemoChangeReporter()
 *   {
 *     button.addPropertyChangeListener(adapter);
 *     button.setLabel("Report This");
 * 
 *     Frame f = new Frame("Demo Change Reporter");
 *     f.setLayout(new FlowLayout());
 *     f.add(button);
 *     f.add(reporter);
 *     f.pack();
 *     f.show();
 *   }
 *   
 *   class PropertyChangeAdapter implements PropertyChangeListener 
 *   {
 *     public void propertyChange(PropertyChangeEvent e)
 *     {
 *       reporter.reportChange(e);
 *     }
 *   }
 * 
 *   public static void main(String[] argv)
 *   {
 *     new DemoChangeReporter();
 *   }
 * }
 * </pre>
 * In the example above, the button is connected to the ChangeReporter with 
 * a (nested) adpater class.  When the DemoChangeReporter object is constructed
 * the buttons label field is set, and the adapters propertyChange method
 * runs.
 *
 */
public class ChangeReporter extends TextField implements Serializable {

    public ChangeReporter() {
	super("", 35);
	setEditable(false);
    }

    public void reportChange(PropertyChangeEvent evt) {
	String text = evt.getPropertyName() + " := " + evt.getNewValue();

	int width = getSize().width - 10;
	Font f = getFont();
	if (f != null) {
	  // Trim the text to fit.
	  FontMetrics fm = getFontMetrics(f);
	  while (fm.stringWidth(text) > width) {
	    text = text.substring(0, text.length()-1);
	  }
	}
				 
	setText(text);
    }

}

