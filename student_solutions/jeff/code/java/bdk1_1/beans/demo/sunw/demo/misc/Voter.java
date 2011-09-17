package sunw.demo.misc;


import java.awt.*;
import java.beans.*;


/** 
 * A simple Java Bean that handles constrained property PropertyChange 
 * events by unconditionally vetoing or accepting all proposed changes.  
 * It can be  used with the JellyBean, which fires vetoable PropertyChange 
 * events,  to demonstrate  constratined properties.  It's easy to 
 * experiment with connecting constratined properties to Voter objects 
 * with the BeanBox.  To do so programatically one would write:
 * 
 * <pre>
 * import sunw.demo.jelly.JellyBean;
 * import sunw.demo.misc.Voter;
 * import java.beans.*;
 *   
 * public class DemoVoter
 * {
 *   JellyBean bean = new JellyBean(); 
 *   Voter voter = new Voter();
 *   VetoableChangeAdapter adapter = new VetoableChangeAdapter();
 *   
 *   DemoVoter()
 *   {
 *     bean.addVetoableChangeListener(adapter);
 *     
 *     try {
 *       bean.setPriceInCents(123); 
 *     }
 *     catch (PropertyVetoException e) {
 *       System.err.println("Vetoed: " + e);
 *     }
 *   }
 *   
 *   class VetoableChangeAdapter implements VetoableChangeListener 
 *   {
 *     public void vetoableChange(PropertyChangeEvent e)
 *       throws PropertyVetoException
 *     {
 *       voter.vetoableChange(e);
 *     }
 *   }
 *   
 *   public static void main(String[] argv)
 *   {
 *     new DemoVoter();
 *   }
 * }
 * </pre>
 * In the example above the nested adapter calls Voter.vetoable change
 * each time a constrained bean property is set.  In this case the 
 * constrained property is "priceInCents".
 * 
 * @see sunw.demo.jelly.JellyBean
 */

public class Voter extends Component {
    private boolean vetoAll = true;    
    private String text = "No";
    private transient int baseline; 

    /** 
     * Construct a Voter that, by default, vetos all proposed PropertyChange events.
     */
    public Voter() {
        setFont(new Font("Helvetica", Font.BOLD, 36));
        setBackground(Color.black);
        setForeground(Color.red);
    }

    /**
     * If true, veto all proposed changes, otherwise accept them.
     * @see #getVetoAll
     * @see #vetoableChange
     */
    public void setVetoAll(boolean x) {
        vetoAll = x;
	if (vetoAll) {
	    text = "No";
	} else {
	    text = "Yes";
	}
	repaint();
    }


    /**
     * If true, veto all proposed changes, otherwise accept them.
     * @see #setVetoAll
     */
    public boolean getVetoAll() {
        return vetoAll;
    }

    /**
     * The PropertyChangeEvent handler method.    By default this method throws 
     * a PropertyVetoException, which vetos the proposed change defined
     * by the PropertyChangeEvent.
     * 
     * @exception PropertyVetoException if the vetoAll is true
     * @see #setVetoAll
     */
    public void vetoableChange(PropertyChangeEvent x)
				throws PropertyVetoException {
        if (vetoAll) {
            throw new PropertyVetoException("NO!", x);
        }
    }


    public Dimension getPreferredSize() {
	FontMetrics fm = getFontMetrics(getFont());
	baseline = fm.getMaxAscent() + 2;
	int height = baseline + fm.getMaxDescent() + 2;
	int width = fm.stringWidth("Yes") + 8;
	return new Dimension(width,height);
    }

    public void paint(Graphics g)  {
	g.setColor(getBackground());
	Dimension size = getSize();
	g.fillRect(0, 0, size.width, size.height);
	g.setColor(getForeground());
	g.setFont(getFont());
	g.drawString(text, 4, baseline);
    }

}
