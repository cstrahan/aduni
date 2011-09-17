package sun.beanbox;

import java.awt.*;
import java.awt.datatransfer.*;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;
import java.lang.reflect.Method;
import java.beans.*;
import javax.infobus.*;

/** 
 * The BeanBox interacts with <code>IBSupport</code> to provide
 * a simplistic example of supporting InfoBus aware Beans.
 *
 * When the BeanBox imports a bean implementing <code>InfoBusMember</code>
 * it registers that bean with <code>IBSupport</code>
 * by calling <code>IBSupport.register</code>.
 */
public class IBSupport implements PropertyChangeListener {

    /**
     * Constructs an <code>IBSupport</code> object.
     */
    IBSupport() {
    }
    
    /**
     * Gets a java.awt.Component displaying information on
     * all available InfoBus's.
     
     * @return an java.awt.Component
     */
    public Component getInfoBusInfoView() {
  	return new IBSupportView(model);
    }
    
    /**
     * Registers an <code>InfoBusMember</code> with this
     * <code>IBSupport</code>.
     * <code>IBSupport</code> adds itself as a
     * <code>PropertyChangeListener</code> on the members' InfoBus
     * property. 
     */
    public void register(InfoBusMember m) {
        m.addInfoBusPropertyListener(this);
        this.add(m);
    }
    
    /**
     * Called when a registered InfoBusMember's InfoBus property
     * changes. The member is disassociated with its old
     * InfoBus and associated with a new InfoBus
     * corresponding to its new InfoBus property value.
     * If the old value is null, we simply associate the member
     * to the new bus. If the new value is null, the 
     * member is still registered, but no longer associated with
     * any InfoBus.
     */  
    public void propertyChange(PropertyChangeEvent evt) {

	// Note: We might get some other property change event.
	// We should therefore check to see that we are in fact 
	// detecting a change on an InfoBusProperty and if not,
	// we should warn and return.

	InfoBusMember member = (InfoBusMember)evt.getSource();
	InfoBus oldIB = (InfoBus)evt.getOldValue();
	InfoBus newIB = (InfoBus)evt.getNewValue();
	  	
	if (oldIB != null) {
		this.remove(member, oldIB.getName());
	}

	if (newIB != null) {
		this.add(member);
	}
    }
    
    protected void add(InfoBusMember m) {
    	model.add(m);
    }
    
    protected void remove(InfoBusMember m, String ibName) {
    	model.remove(m,ibName);
    }   
    
    public String toString() {
    	return model.toString();
    }
    
    private IBModel model = new IBModel();
}

//--------------------------------------------------------------------

/** 
 * Keeps track of <code>InfoBus</code>/<code>InfoBusMember</code>
 * relationships.
 */
class IBModel {
    /**
     * Associates an <code>InfoBusMember</code> with an InfoBus.
     */     
    void add(InfoBusMember member) {

	InfoBus ib = member.getInfoBus();

	if (ib == null) {
	    System.out.println("WARING: Adding a member w/out an InfoBus is a no-op."); 
	    return;
	}

	String ibName = ib.getName();

	// check to see if ibName is already exists
	if (busMembership.containsKey(ibName)) {
	    Vector v = (Vector)busMembership.get(ibName);
	    //check to see if member is already registered with ibName
	    if (v != null) {
		if (v.contains(member)) {
		    System.out.println("WARNING: "  
					+ member.getClass().getName() 
					+ " is already on " 
					+ ibName 
					+ " bus");
		} else {
		    //register member with ibName
		    v.addElement((Object)member);
		}		
	    }
	} else {
	    // A new InfoBus has been created. Add the new member.
	    Vector v = new Vector();
	    v.addElement((Object)member);
	    busMembership.put(ibName, v);

	    // Attatch an InfoBusDataController to monitor InfoBus
	    // traffic.
	    try {
	    	IBMonitor m = new IBMonitor();
	        ib.addDataController(m, InfoBus.MONITOR_PRIORITY );
	    	busMonitors.put(ibName, m);
	    } catch (Exception e) { System.out.println(e); }

	    fireAction(); // notify views to update
	}
    }

    /**
     * Disassociates an InfoBusMember with an InfoBus.
     */
    void remove(InfoBusMember member, String ibName) {

	// if busName & member exist, disassociate member from bus,
	// otherwise warn.

	if (busMembership.containsKey(ibName)) {
	    Vector v = (Vector)busMembership.get(ibName);

	    if (v != null) {
		if (!v.contains(member)) {
		    System.out.println("WARNING: "  
					+ member.getClass().getName() 
					+ " is not already on " 
					+ ibName 
					+ " bus");
		} else {
		    v.removeElement((Object)member);
		}

		// If there are no more InfoBus components on this bus,
		// remove it.
		if (v.isEmpty()) {
		    busMembership.remove(ibName);
		    fireAction(); // notify views to update
		}
	    }
	} else {
	    System.out.println("WARNING: Cannot remove "  
					+ member.getClass().getName() 
					+ " from bus "
					+ ibName 
					+ "; bus does not exist.");
	}
    }   

    /**
    * Adds the specified action listener to receive action events from
    * this InfoBus model. Action events occur when a new InfoBus is
    * created.
    */ 
    public synchronized void addActionListener(ActionListener l) {
		actionListeners.addElement(l);
    }

    /**
     * Removes the specified action listener so that it no longer 
     * receives action events from this InfoBus model. Action events
     * occur when an InfoBus loses its last member.
     */ 
    public synchronized void removeActionListener(ActionListener l) {
		actionListeners.removeElement(l);	
    }
    
    /**
     * Fires an action event to signal the addition or removal
     * of an available InfoBus.
     */ 
    public void fireAction() {
	Vector targets;
	synchronized (this) {
	    targets = (Vector) actionListeners.clone();
	}
	ActionEvent actionEvt = new ActionEvent(this, 0, null);
	for (int i = 0; i < targets.size(); i++) {
	    ActionListener target = (ActionListener)targets.elementAt(i);
	    target.actionPerformed(actionEvt);
	}
    }
     
    /**
     * Gets the names of all available InfoBus instances.
     *
     * @return an <code>Enumeration</code> of <code>String</code>s
     * corresponding to InfoBus names
     */
    Enumeration getInfoBusNames() {
	return busMembership.keys();
    }
    
    /**
     * Gets references to all available InfoBusMembers for a
     * particular InfoBus.
     *
     * @return an <code>Enumeration</code> of <code>InfoBusMembers</code>s
     */
     Enumeration getInfoBusMembers(String ibName) {
        Vector v = (Vector)busMembership.get(ibName);
	
	if (v == null) {
	    v = new Vector();
	}
	
	return v.elements();
    }
    
    /**
     * Gets a java.awt.Component displaying <code>InfoBusEvent</code>
     * traffic on a particular InfoBus.
     *
     * @return an java.awt.Component
     */
    Component getInfobusTrafficView(String ibName) {
    	return (Component)((IBMonitor)busMonitors.get(ibName)).getTextArea();
    }

    public String toString() {
    	return busMembership.toString();
    }
    
    private Vector actionListeners = new Vector();   
    private Hashtable busMembership = new Hashtable();
    private Hashtable busMonitors = new Hashtable();
}

//--------------------------------------------------------------------

class IBSupportView extends Container {
	
    IBSupportView(IBModel model) {
	setSize(350, 300);
	setLayout(new BorderLayout());
	
	Panel c = new Panel();
	c.setLayout(new BorderLayout());
	IBBusList bList = new IBBusList(model);
	c.add(bList, "West");

	IBMemberList mList = new IBMemberList(model);
	c.add(mList, "East");

	Label l = new Label("Change bus for selected beans to:");
	TextField t = new TextField("", 20);
	Panel p = new Panel();
	p.add(l);
	p.add(t);
	c.add(p, "South");
	
	add(c, "North");
	
	IBTrafficLog traffic = new IBTrafficLog(model);
	add(traffic, "Center");
	traffic.repaint();
	
	// attatch listeners
	model.addActionListener(bList);//changes to model affect bus list
	bList.addItemListener(mList);//bus list selection affects member list
 	t.addActionListener(mList);//text field change affects member list
 	
 	bList.addItemListener(traffic);//bus list selection affects traffic view
   } 
     
}

//--------------------------------------------------------------------

class IBBusList extends List implements ActionListener  {
    IBBusList(IBModel model) {
    	this.model = model;
    	display();
    }
    
    /**
     * Displays available InfoBuses in this list.
     */
    protected void display() {
    	removeAll();
    	
	for (Enumeration e = model.getInfoBusNames(); e.hasMoreElements(); ) {
	    String ibName = (String)e.nextElement();
	    add(ibName);
	}
    }
  
    /**
     * Updates the IBBusList display.
     * Called when an InfoBus is created or destroyed.
     */    
    public void actionPerformed(ActionEvent e) {
	display();
    }
    
    private IBModel model;
}

//--------------------------------------------------------------------

class IBMemberList extends List implements ItemListener, ActionListener {

    IBMemberList(IBModel model) {
    	this.model = model;
    	setMultipleMode(true);
    }
       
    /** 
     * Gets the name of the InfoBus currently displayed by this list.
     */     
    String getIBName() {
    	return ibName;
    }

    /** 
     * Sets the name of the InfoBus to currently display in this list.
     */     
    void setIBName(String ibName) {
    	 this.ibName = ibName;
    }
       
    /**
     * Gets an <code>Enumeration</code> of <code>InfoBusMember</code>s
     * corresponding to the InfoBus currently displayed.
     */
    Enumeration getInfoBusMembers() {
    	String ibName = getIBName();
    	
        // We don't get a Vector back from the model directly, so we'll
        // create one for our own internal use. We'll want to order
        // the list of InfoBusMembers with a Vector so we can match their
        // display names directly with user selected indices.
        
   	this.v = new Vector(); // throw out the old Vector.
     	
     	if (ibName != null) {    	
     	    for (Enumeration e = model.getInfoBusMembers(ibName); e.hasMoreElements(); ) {
	        v.addElement(e.nextElement());
	    }
	}
	
	return v.elements();
    }
    
    /**
     * Display <code>InfoBusMember</code> names in this list.
     */
    protected void display() {
    	removeAll();
    	
	for (Enumeration e = this.getInfoBusMembers(); e.hasMoreElements(); ) {
	    Object bean = e.nextElement();

	    BeanInfo bi;
	    try {
	    
		bi = Introspector.getBeanInfo(bean.getClass());		
		add(bi.getBeanDescriptor().getDisplayName());
		
	    } catch (Exception ex) {
		System.out.println("Caught: " + ex);
		System.out.println("While displaying: " + bean);
	    }
	}
    }
  
    /**
     * Updates the IBMemberList display.
     * Called when the user selects an InfoBus name from the
     * InfoBusViewList to inspect.
     */
    public void itemStateChanged(ItemEvent e) {
	if (e.getStateChange() == ItemEvent.SELECTED) {
	    String ibName = ((List)e.getItemSelectable()).getSelectedItem();
	    setIBName(ibName); // updates which InfoBus to display.
	    display();
	}
    }
     
    /** 
     * Sets the InfoBusName property of all the currently selected
     * InfoBusMembers in this List.
     */
    public void actionPerformed(ActionEvent e) {

    	String newBus = e.getActionCommand();
	
	if (newBus.equals("")) {
	    newBus = null;
	}
		
	int[] indicies = getSelectedIndexes();
	
	if (indicies != null) {
	    for (int i = 0; i < indicies.length; i++) {
		Class c = v.elementAt(indicies[i]).getClass();
		Method[] methods = c.getMethods();
		
		for (int k = 0; k < methods.length; k++) {
		
		    Method m = methods[k];
		    
		    boolean correctName = m.getName().equals("setInfoBusName");
		    boolean correctReturnType = (m.getReturnType() == void.class);
		    boolean hasOnlyOneParam =(m.getParameterTypes().length == 1);
		    boolean paramIsString = false;
		    
		    if (hasOnlyOneParam) {
		        paramIsString = (m.getParameterTypes()[0] == String.class);
		    }
		    
		    boolean correctParamTypes = hasOnlyOneParam & paramIsString;
		    
	   		if (correctName && correctParamTypes && correctReturnType) {		
			    try {
				String[] params = {new String(newBus)};			
				m.invoke((Object)v.elementAt(indicies[i]), (Object[])params);
			    } catch (Exception ex) {
				System.out.println(c.getName() + ".setInfoBusName() threw " +ex);		
			    }
		        }
		    } // methods
	        } // indicies
	    display(); // redisplay the updated list
	} else { 
	    // silently ignore - (indicies == null) is true.
	}
    }

    private IBModel model;
    private Vector v = new Vector();
    private String ibName;
}

  
//--------------------------------------------------------------------

class IBTrafficLog extends Panel implements ItemListener {

// waits for ibName to come in through ItemListenerEvent...

    IBTrafficLog(IBModel model) {
       	this.model = model;
    	
	setSize(300, 100);
	setLayout(new BorderLayout());
	display();
    }
    
    /** 
     * Gets this components preferred <code>Dimension</code>.
     */     
    public Dimension getPreferredSize() {
    	return new Dimension(300,100);
    }

    /** 
     * Gets the name of the InfoBus currently displayed.
     */     
    String getIBName() {
    	return ibName;
    }

    /** 
     * Sets the name of the InfoBus to display.
     */     
    void setIBName(String ibName) {
       	if (ibName != null) {
    		this.ibName_old = this.ibName;
    		this.ibName = ibName;   		
    	}
    }

    /**
     * Updates the IBTrafficLog display.
     * Called when an IBBusList item is selected.
     */    
    public void itemStateChanged(ItemEvent e) {
	if (e.getStateChange() == ItemEvent.SELECTED) {
	    String ibName = ((List)e.getItemSelectable()).getSelectedItem();
	    setIBName(ibName); // updates which InfoBus to display.
	    display();
	}
    }

    void display() {
       	if (ibName == null) {
    		displayArea = new TextArea("No Infobus is currently selected.");
    		((TextArea)displayArea).setEditable(false);
	    	add(displayArea, "West");
    	}
    	else if (!ibName.equals(ibName_old)) { // name has changed...
    		displayArea_old = displayArea;
    		displayArea = (TextArea)model.getInfobusTrafficView(ibName);
 	    	if (displayArea_old != null) remove(displayArea_old);
    		((TextArea)displayArea).setEditable(false);
 	   	add(displayArea, "West");
   	}
    	
     	if (getParent() != null) getParent().validate();
 	displayArea.repaint();
   }
    
   public void update(Graphics g) {
   }
    
   
   private Component displayArea;
   private Component displayArea_old;
   private IBModel model;
   private String ibName;
   private String ibName_old;

}

//--------------------------------------------------------------------

class IBMonitor implements InfoBusDataController {

    IBMonitor() {
	textArea = new TextArea(this + "\n");
    }

    TextArea getTextArea() {
	return textArea;
    }

    public void setConsumerList ( Vector consumers ) {
    }
	
    public void setProducerList ( Vector producers ){
    }
	
    public void addDataConsumer ( InfoBusDataConsumer consumer ){
	textArea.append("addDataConsumer: " + consumer + "\n");
    }
	
    public void addDataProducer ( InfoBusDataProducer producer ){
	textArea.append("addDataProducer: " + producer + "\n");
    }
	
    public void removeDataConsumer ( InfoBusDataConsumer consumer ){
	textArea.append("removeDataConsumer: " + consumer + "\n");
    }
	
    public void removeDataProducer ( InfoBusDataProducer producer ){
	textArea.append("removeDataProducer: " + producer + "\n");
    }
	
    public boolean fireItemAvailable ( String dataItemName, DataFlavor[] flavors, InfoBusDataProducer source ){	
	textArea.append("fireItemAvailable: source: " + source + "\n" );
	textArea.append("data item name: " + dataItemName + "\n" );		
	if (flavors != null) {
		for (int i = 0; i < flavors.length; i++) {
		textArea.append("flavors["+i+"]: " + flavors[i] + "\n" );			
		}		
	}

	return false;
    }
	
    public boolean fireItemRevoked ( String dataItemName, InfoBusDataProducer producer ) {
	textArea.append("fireItemRevoked: producer: " + producer + "\n");
	textArea.append("data item name: " + dataItemName + "\n" );		
	return false;
    }
	
    public boolean findDataItem ( String dataItemName, DataFlavor[] flavors,InfoBusDataConsumer consumer, Vector foundItem) {
	textArea.append("findDataItem: consumer: " + consumer  + "\n");
	textArea.append("data item name: " + dataItemName + "\n" );		
	if (flavors != null) {
		for (int i = 0; i < flavors.length; i++) {
		textArea.append("flavors["+i+"]: " + flavors[i] + "\n" );			
		}		
	}
	return false;
    }
	
    public boolean findMultipleDataItems ( String dataItemName, DataFlavor[] flavors,InfoBusDataConsumer consumer, Vector foundItems) {
	textArea.append("findMultipleDataItems: consumer: " + consumer+ "\n" );
	textArea.append("data item name: " + dataItemName + "\n" );		
	if (flavors != null) {
		for (int i = 0; i < flavors.length; i++) {
		textArea.append("flavors["+i+"]: " + flavors[i] + "\n" );			
		}		
	}
	return false;
    }

    private TextArea textArea;

}
