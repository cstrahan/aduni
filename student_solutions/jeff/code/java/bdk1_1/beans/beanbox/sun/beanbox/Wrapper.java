
package sun.beanbox;

/**
 * This wrapper class keeps track of various BeanBox related
 * state for each bean in the composition window.
 *
 * Among other things, it draws the black-and-white hashed
 * border around the currenty active component.
 */

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.beans.*;
import java.util.*;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.applet.Applet;
import sunw.beanbox.PropertyHookup;

public class Wrapper extends Panel implements Serializable, 
				MouseListener, MouseMotionListener {

    static final long serialVersionUID = 1144602051002987355L;

    public Wrapper(Object bean, String beanLabel, String beanName) {
        this.bean = bean;
	if (beanName == null) {
	    beanName = bean.getClass().getName();
	}
	if (beanLabel == null) {
	    beanLabel = beanName;
	}
	this.beanName = beanName;
	this.beanLabel = beanLabel;
	this.isFromPrototype = false;
	
	setLayout(null);
	if (Beans.isInstanceOf(bean, Component.class)) {
            child = (Component)Beans.getInstanceOf(bean, Component.class);
	} else {
	    invisibleWrappers.addElement(this);
	    child = new OurLabel(beanLabel);
	    if (BeanBoxFrame.getHideInvisibleBeans()) {
		setVisible(false);
	    }
	}
	add(child);
	child.setLocation(borderWidth, borderWidth);
	initialize();
	attachListeners();
    }

    void initialize() { 
	if (! Beans.isInstanceOf(bean, Component.class)) {
	    invisibleWrappers.addElement(this);
	    if (BeanBoxFrame.getHideInvisibleBeans()) {
		setVisible(false);
	    }
	}
	esdMap = new Hashtable();
	try {
	    BeanInfo bi = Introspector.getBeanInfo(bean.getClass());
	    EventSetDescriptor[] esds = bi.getEventSetDescriptors();
	    for (int i = 0; i < esds.length; i++) {
	        esdMap.put(esds[i].getName(), esds[i]);
	    }
	} catch (IntrospectionException ex) {
	    System.err.println("Wrapper couldn't introspect on bean: " + bean);
        }
	if (eventTargets == null) {
	    eventTargets = new Vector();
	    propertyTargets = new Vector();
	} else {
	    propertyTargets = getWPEIfromWET(eventTargets);
	}
    }

    /**
     * This method massages information already present in WrapperEventTarget
     * into Wrapper(Property)EventInfo, which is easier to use.
     */

    private Vector getWPEIfromWET(Vector wets) {
	Vector back = new Vector();
	// we only care about properties, not method hookups
	for (Enumeration e = wets.elements();
	     e.hasMoreElements(); ) {
            WrapperEventTarget wet = (WrapperEventTarget) e.nextElement();
	    if (wet.targetListener instanceof PropertyHookup) {

		PropertyHookup h = (PropertyHookup) wet.targetListener;
                Hashtable table = h.getTargetsByProperty();
		for (Enumeration keys = table.keys();
		     keys.hasMoreElements(); ) {
		    String propertyName = (String) keys.nextElement();
		    Vector targets = (Vector) table.get(propertyName);

		    for (Enumeration ee = targets.elements();
			 ee.hasMoreElements(); ) {
			Object t = ee.nextElement();

			back.addElement(
			     new
			     WrapperPropertyEventInfo(
						      h.getTargetObject(t),
						      propertyName,
						      h.getSetterMethod(t)));
		    }
		}
	    }
	}
	return back;
    }


    /**
     * Serialization methods
     */

    private void readObject(ObjectInputStream s)
        		throws ClassNotFoundException, IOException {
	s.defaultReadObject();
	initialize();
    }

    synchronized void addPropertychangeListener(String propertyName,
				PropertyChangeListener listener) {
    }

    /**
     * How many Hookups?
     */
    int getEventHookupCount() {
	if (propertyTargets.size() > 0) {
	    return eventTargets.size() + propertyTargets.size() - 1;
	} else {
	    return eventTargets.size();
	}
    }

    public String getAdderName(String eventSetName) {
	EventSetDescriptor esd = (EventSetDescriptor) esdMap.get(eventSetName);
	Method adder;
	adder = esd.getAddListenerMethod();
	return adder.getName();
    }

    public String getRemoverName(String eventSetName) {
	EventSetDescriptor esd = (EventSetDescriptor) esdMap.get(eventSetName);
	Method remover;
	remover = esd.getRemoveListenerMethod();
	return remover.getName();
    }


    /**
     * This will replace the bottom two
     */
    public WrapperEventInfo[] getEventHookupInfo() {
	int s = getEventHookupCount();

	WrapperEventInfo[] back = new WrapperEventInfo[s];
	int i = 0;
	Enumeration e = eventTargets.elements();
	while (e.hasMoreElements()) {
	    WrapperEventTarget et = (WrapperEventTarget) e.nextElement();
	    if (et.targetBean == null) {
		// this is a property bound hookup
	    } else {
		back[i] = new WrapperEventInfo(et.targetBean,
					       et.targetListener.getClass().getName(),
					       et.eventSetName);
		i += 1;
	    }
	}
	e = propertyTargets.elements();
	while (e.hasMoreElements()) {
	    back[i] = (WrapperEventInfo) e.nextElement();
	    i += 1;
	}
	return back;
    }


    // Add a (set of) PropertyHookup
    synchronized void addPropertyTarget(String propertyName,
					Object targetObject, Method setter) {
	propertyTargets.addElement(
            new WrapperPropertyEventInfo(targetObject,
					 propertyName, setter));
    }

    // Add a hookup.  All property bound hookups are represented by (at most) one hookup
    synchronized void addEventTarget(String eventSetName,
			Wrapper targetWrapper, Object listener) {
	WrapperEventTarget et = new WrapperEventTarget();
	et.eventSetName = eventSetName;
	if (targetWrapper != null) {
	    et.targetBean = targetWrapper.getBean();
	}
	et.targetListener = listener;
	eventTargets.addElement(et);
	EventSetDescriptor esd = (EventSetDescriptor) esdMap.get(eventSetName);
	if (esd == null) {
	    System.err.println("Internal error: Wrapper.addEventTarget missing event set");
	    System.err.println("        eventSetName = " + eventSetName);
	    System.err.println("        bean = " + bean);
	    return;
	}
	Method adder = esd.getAddListenerMethod();
	Method remover = esd.getRemoveListenerMethod();
	if (adder == null || remover == null) {
	    System.err.println("Internal error: Wrapper.addEventTarget missing add/remote listener");
	    System.err.println("        eventSetName = " + eventSetName);
	    System.err.println("        bean = " + bean);
	    return;
	}
	try {
	    Object args[] = { listener };
	    adder.invoke(bean, args);
	} catch (InvocationTargetException ex) {
	    System.err.println("Wrapper: adding event listener for " + eventSetName + " failed:");
	    System.err.println("    " + ex.getTargetException());
	} catch (Exception ex) {
	    System.err.println("Wrapper: adding event listener for " + eventSetName + " failed:");
	    System.err.println("    " + ex);
	}
    }

    /**
     * Temporarily remove any event listeners.
     */

    void removeListeners() {
	Enumeration enum = eventTargets.elements();
	while (enum.hasMoreElements()) {
	    WrapperEventTarget et = (WrapperEventTarget)enum.nextElement();
	    EventSetDescriptor esd = (EventSetDescriptor) esdMap.get(et.eventSetName);
	    Method remover = esd.getRemoveListenerMethod();
	    try {
	        Object args[] = { et.targetListener };
	        remover.invoke(bean, args);
	    } catch (InvocationTargetException ex) {
	        System.err.println("Wrapper: removing event listener for "
					 + et.eventSetName + " failed:");
	        System.err.println("    " + ex.getTargetException());
		ex.getTargetException().printStackTrace();
	    } catch (Exception ex) {
	        System.err.println("Wrapper: removing event listener for "
					 + et.eventSetName + " failed:");
	        System.err.println("    " + ex);
		ex.printStackTrace();
	    }
	}
	// Remove mouse listeners.
	listenForMice(false);
    }

    /**
      * (Re)-attach any event listeners.
      */
    void attachListeners() {   
	Enumeration enum = eventTargets.elements();
	while (enum.hasMoreElements()) {
	    WrapperEventTarget et = (WrapperEventTarget)enum.nextElement();
	    EventSetDescriptor esd = (EventSetDescriptor) esdMap.get(et.eventSetName);
	    Method adder = esd.getAddListenerMethod();
	    try {
	        Object args[] = { et.targetListener };
	        adder.invoke(bean, args);
	    } catch (InvocationTargetException ex) {
	        System.err.println("Wrapper: adding event listener for "
					 + et.eventSetName + " failed:");
	        System.err.println("    bean = " + bean);
	        System.err.println("    " + ex.getTargetException());
		ex.getTargetException().printStackTrace();
	    } catch (Exception ex) {
	        System.err.println("Wrapper: adding event listener for "
					 + et.eventSetName + " failed:");
	        System.err.println("    bean = " + bean);
	        System.err.println("    " + ex);
		ex.printStackTrace();
	    }
	}
	// Reattach mouse listeners.
	listenForMice(true);
    }


    void writeNakedBean(ObjectOutputStream oos) throws IOException {

	// First, detach all event listeners.
	removeListeners();
        			
	try {
	    // Now write the bean.
	    oos.writeObject(bean);
	} finally {
	    // Now rettach all event listeners.
	    attachListeners();
	}
    }

    /**
     * Cleanup is called when a Wrapper has been "cut" from the BeanBox.
     */
    void cleanup() {
	if (bean instanceof Applet) {
	    Applet apl = (Applet)bean;
	    apl.stop();
	    apl.destroy();
	}
	removeListeners();	
	// We should also remove ourself from any event sources...
    }

    /**
     * set whether or not the target bean is from a serialized origin
     */
    public void setFromPrototype(boolean b)
    {
	isFromPrototype = b;
    }
	 
    /**
     * get whether or not the target bean is from a serialized origin
     */
    public boolean isFromPrototype()
    {
	return isFromPrototype;
    }

    /**
     * get the wrapped bean.
     */
    public Object getBean() {
	return bean;
    }

    /**
     * get the wrapped beanName
     */
    public String getBeanLabel() {
	return beanLabel;
    }

    /**
     * get the wrapped beanName
     */
    public String getBeanName() {
	return beanName;
    }

    /**
     * get the AWT component used to represent the wrapped bean.
     */
    public Component getChild() {
	return child;
    }

    /**
     * get the properties changed at design time.
     */
    public Vector getChangedProperties()
    {
	 if (changedProperties == null) {
	        changedProperties = new Vector();
	}
	 return changedProperties;
    }
	 
    /**
     * Static method to map from an AWT component to the associated Wrapper.
     */
    public static Wrapper findWrapper(Component comp) {
	for (;;) {
	    if (comp == null) {
	        throw new Error("component has no wrapper!?");
	    }
	    if (comp instanceof Wrapper) {
		return (Wrapper) comp;
	    }
	    if (comp instanceof BeanBoxFrame) {
		return BeanBoxFrame.getTopWrapper();
	    }
	    comp = comp.getParent();
	}
    }

    public void doLayout() {
	// Has the child gotten bigger?  If so, expand to fit.
	Dimension d = getSize();
	Dimension cd = child.getMinimumSize();
	if (cd.width > (d.width - (2*borderWidth)) ||
		cd.height > (d.height - (2*borderWidth))) {
	    int width = d.width;
	    if (cd.width > (d.width - (2*borderWidth))) {
		width = cd.width + (2*borderWidth);
	    }
	    int height = d.height;
	    if (cd.height > (d.height - (2*borderWidth))) {
	        height = cd.height + (2*borderWidth);
	    }
	    setSize(width,height);
	}
    }

    public void setActive(boolean isActive) {
	active = isActive;
	repaint();
    }

    public Dimension getPreferredSize() {
	Dimension childSize = child.getPreferredSize();
	if (childHasStupidPreferredSize()) {
	    childSize = child.getSize();
	}
	int width = childSize.width;
	int height = childSize.height;
	// Make sure the child is at least its minimum size.
	Dimension minSize = child.getMinimumSize();
	if (minSize.height > height) {
	    height = minSize.height;
	}
	if (minSize.width > width) {
	    width = minSize.width;
	}
	width += (2 * borderWidth);
	height += (2 * borderWidth);
	return new Dimension(width, height);
    }

    private boolean childHasStupidPreferredSize() {
	// We do a special check for demented behaviour from empty Panels
	// for the sake of applets.  If an applet does not explicitly
	// specify a dimension, then we allow it to be resized arbitrarily
	// rather than limiting it to the tiny size returned by the FlowLayout
	if (child instanceof Panel) {
	    Container cont = (Container)child;
	    LayoutManager lay = cont.getLayout();
	    if (cont.getComponentCount() == 0 && lay instanceof FlowLayout) {
		FlowLayout flow = (FlowLayout) lay;
		Dimension cd = child.getPreferredSize();
		Dimension fd = flow.preferredLayoutSize(cont);
		if (cd.width == fd.width && cd.height == fd.height) {
		    return true;
		}
	    }
	}
	return false;
    }

    public void setBounds(int x, int y, int width, int height) {

	// If we're the top level wrapper, there is no dickering.
	if (getParent() != null && getParent() instanceof Frame) {
	    super.setBounds(x,y,width,height);
	    child.setBounds(borderWidth, borderWidth,
		width-(2*borderWidth), height-(2*borderWidth));
	    child.validate();
	    return;
	}

	// Figure out what size we want to set the child 
	width -= (2 * borderWidth);
	height -= (2 * borderWidth);

	// Make sure the child is at least its minimum size.
	Dimension minSize = child.getMinimumSize();
	if (minSize.height > height) {
	    height = minSize.height;
	}
	if (minSize.width > width) {
	    width = minSize.width;
	}

	// Make sure the child is under its maximum size.
	Dimension maxSize = child.getMaximumSize();
	if (height > maxSize.height) {
	    height = maxSize.height;
	}
	if (width > maxSize.width) {
	    width = maxSize.width;
	}

	// Now we can set the child's size.
	child.setBounds(borderWidth, borderWidth, width, height);

	// Finally we can set our own size.
	width += (2 * borderWidth);
	height += (2 * borderWidth);
	super.setBounds(x, y, width, height);

	child.validate();
    }

    // Note that to avoid deadlocks, paint is *not* synchronized
    public void paint(Graphics g) {
	if (active && Beans.isDesignTime()) {
	    int width = getSize().width;
	    int height = getSize().height;

	    getHashBars(this);

	    // Draw the bounding hasbox as a set of images.

	    // First draw the top and bottom bars.
	    int nudge = 2 * hashBarWidth;
	    int bottomNudge = - ((height-hashBarWidth) % nudge);
	    for (int x = 0; x < width; x += hashBarLength) {
	        g.drawImage(xHashBar, x, 0, null);
	        g.drawImage(xHashBar, x + bottomNudge, height-hashBarWidth, null);
	    }
	    // Now draw the left and right bars.
	    int rightNudge = - ((width-hashBarWidth) % nudge);
	    for (int y = 0; y < height; y += hashBarLength) {
	        g.drawImage(yHashBar, 0, y, null);
	        g.drawImage(yHashBar, width-hashBarWidth, y+rightNudge, null);
	    }


	}
	super.paint(g);
    }

    public synchronized void activate() {
	active = true;
	repaint();
    }
 
    public synchronized void deactivate() {
	active = false;
	repaint();
    }

   public synchronized void doMouseStuff(MouseEvent evt) {
	// Code to handle drawing the resize/move cursor.

	if (!active || !Beans.isDesignTime()) {
	    return;
	}
	Component parent = getParent();

	// You can't move or resize the top-level beanbox.
	if (parent != null && parent instanceof BeanBoxFrame) {
	    return;
	}

	int x = evt.getX();
	int y = evt.getY();
  	int id = evt.getID();

	int lowX = borderWidth;
	int highX = getSize().width - borderWidth;
	int lowY = borderWidth;
	int highY = getSize().height - borderWidth;
	boolean onTheBorder = ((evt.getSource() == this) &&
			(x < lowX || x > highX || y < lowY || y > highY));

	Cursor newCursor = defaultCursor;

	if (onTheBorder) {
	    if (id == MouseEvent.MOUSE_PRESSED) {
	        // Check if we need to resize or move.
	        if (cursor == nwResizeCursor || 
		    cursor == swResizeCursor ||
		    cursor == neResizeCursor ||
		    cursor == seResizeCursor) {
		    getBeanBox().startResize(this, x, y, cursor);
	        } else if (cursor == moveCursor) {
		    getBeanBox().startMove(this, x, y);
	        } 

	    } else if (id == MouseEvent.MOUSE_ENTERED 
				|| id == MouseEvent.MOUSE_MOVED) {

		// If we're in a corner set the cursor to indicate a
		// resize, otherwise a move.
		lowX += resizeDelta;
		lowY += resizeDelta;
		highX -= resizeDelta;
		highY -= resizeDelta;

	        if (x < lowX && y < lowY) {
		    newCursor = nwResizeCursor;
	        } else if (x < lowX && y > highY) {
		    newCursor = swResizeCursor;
	        } else if (x > highX && y < lowY) {
		    newCursor = neResizeCursor;
	        } else if (x > highX && y > highY) {
		    newCursor = seResizeCursor;
		} else {
		    newCursor = moveCursor;
		}
	    }
	}

	if (newCursor != cursor) {
	    cursor = newCursor;
	    // To workaround a bug on JDK 1.1, we avoid changing the user
	    // visible cursor on a straight JDK 1.1 system.
	    if (!isJDK("1.1_Final")) {
		setCursor(cursor);
	    }
	}
    }

    final BeanBox getBeanBox() {
	Component parent = getParent();
	if (parent instanceof BeanBox) {
	    return (BeanBox)parent;
	}
	// Hmm, our parent isn't a BeanBox.  We may be the
	// top-level wrapper.
	if (parent instanceof BeanBoxFrame && child instanceof BeanBox) {
	    return (BeanBox) child;
	}
	// Otherwise we're hopelessly confused.
	System.err.println("Warning: Can't find BeanBox from wrapper");
	// System.err.println("    this = " + this);
	// System.err.println("    child = " + child);
	// System.err.println("    parent = " + getParent());
	// System.err.println("    child.parent = " + child.getParent());
	// Since we don't know who we are, punt to the top-level BeanBox.
	return BeanBoxFrame.getTopBox();
    }

    /**
     * show or hide all the "invisible beans".
     */
    static void showInvisibleBeans(boolean show)  {
	for (int i = 0; i < invisibleWrappers.size(); i++) {
	    ((Wrapper)invisibleWrappers.elementAt(i)).setVisible(show);
	}
    }

    //----------------------------------------------------------------------

    // Mouse listener methods for target bean.
    // We also report all mouse events to our containing BeanBox.

    public void mouseClicked(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseClicked(evt);
    }

    public void mousePressed(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mousePressed(evt);
    }

    public void mouseReleased(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseReleased(evt);
    }

    public void mouseEntered(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseEntered(evt);
    }

    public void mouseExited(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseExited(evt);
    }

    public void mouseDragged(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseDragged(evt);
    }

    public void mouseMoved(MouseEvent evt) {
	doMouseStuff(evt);
	getBeanBox().mouseMoved(evt);
    }

    //----------------------------------------------------------------------

    /**
     * @deprecated  Provided for backwards compatibility only.
     * We support old-style event handling as a backward compatibility
     * feature when we're wrapping a bean using the old event model.
     */
    
    public boolean handleEvent(Event evt) {
	Component source = (Component) evt.target;
	MouseEvent me;
	int x = evt.x;
	int y = evt.y;
	if (source == child) {
	   // remap coordinate to child's space.
	    x = x - borderWidth;
	    y = y - borderWidth;
	}
	if (evt.id == Event.MOUSE_DOWN) {
	    me = new MouseEvent(source, MouseEvent.MOUSE_PRESSED,
			0, 0, x, y, 0, false);
	    mousePressed(me);
	} else if (evt.id == Event.MOUSE_UP) {
	    if (sawMouseDown) {
	        me = new MouseEvent(source, MouseEvent.MOUSE_CLICKED,
			0, 0, x, y, 0, false);
	        mouseClicked(me);
	    }
	    sawMouseDown = false;
	    me = new MouseEvent(source, MouseEvent.MOUSE_RELEASED,
			0, 0, x, y, 0, false);
	    mouseReleased(me);
	} else if (evt.id == Event.MOUSE_MOVE) {
	    me = new MouseEvent(source, MouseEvent.MOUSE_MOVED,
			0, 0, x, y, 0, false);
	    mouseMoved(me);
	} else if (evt.id == Event.MOUSE_ENTER) {
	    sawMouseDown = false;
	    me = new MouseEvent(source, MouseEvent.MOUSE_ENTERED,
			0, 0, x, y, 0, false);
	    mouseEntered(me);
	} else if (evt.id == Event.MOUSE_EXIT) {
	    sawMouseDown = false;
	    me = new MouseEvent(source, MouseEvent.MOUSE_EXITED,
			0, 0, x, y, 0, false);
	    mouseExited(me);
	} else if (evt.id == Event.MOUSE_DRAG) {
	    me = new MouseEvent(source, MouseEvent.MOUSE_DRAGGED,
			0, 0, x, y, 0, false);
	    mouseDragged(me);
	}
	return false;
    }

    //----------------------------------------------------------------------
    private static Hashtable eventModelCache = new Hashtable();

    // Check if our child needs to use the old AWT event mnodel.

    private synchronized boolean useNewEventModel() {
	// Check our cache first.
	Boolean b = (Boolean) eventModelCache.get(child.getClass());
	if (b != null) {
	    return b.booleanValue();
	}

	// We check whether the bean has any public methods that take
	// old AWT event objects.  If so, we assume it wants the
        // old event model, otherwise we assume it wants the new.
	boolean useNew = true;

	try {
	    Class clz = child.getClass();
	    while (useNew && clz != null) {

		java.lang.reflect.Method methods[] = clz.getDeclaredMethods();

		for (int i = 0; i < methods.length; i++) {
		    java.lang.reflect.Method m = methods[i];
		    int mods = m.getModifiers();
		    if (!java.lang.reflect.Modifier.isPublic(mods)) {
			// Skip non=public methods.
			continue;
		    }
		    Class params[] = m.getParameterTypes();
		    if (params.length > 0 && params[0] == java.awt.Event.class) {
			// First arg is java.awt.Event.  We assume this is an
			// old-style AWT event handler method.
			useNew = false;
			break;
		    }
		}
		clz = clz.getSuperclass();
		if (clz.getName().indexOf("java.") == 0) {
		   // We've reached a java.* class, so we're done.
		    break;
	  	}
	    }
	} catch (Exception ex) {
	    System.err.println("Wrapper.useOldEventModel caught: " + ex);
	}

	eventModelCache.put(child.getClass(), new Boolean(useNew));
	return useNew;
    }


    void listenForMice(boolean enable) {
	// If the child uses the old event mode we rely on receiving events
	// through handleEvent.  Otherwise we use the new event model and
	// register explicit listeners for mouse events.
	if (!useNewEventModel()) {
	    System.err.println("WARNING: \"" 
							+ child.getClass().getName()
							+ "\" "
							+ "is a transitional bean.\n"
							+ "SOME BEAN CONTAINERS MAY NOT SUPPORT"
							+ " TRANSITIONAL BEANS!"
							);
	    return;
	}
	if (enable) {
   	    this.addMouseListener(this);
	    this.addMouseMotionListener(this);
	    child.addMouseListener(this);
	    child.addMouseMotionListener(this);
	} else {
   	    this.removeMouseListener(this);
	    this.removeMouseMotionListener(this);
	    child.removeMouseListener(this);
	    child.removeMouseMotionListener(this);
	}
    }

    private static synchronized void getHashBars(Component c) {
	if (xHashBar != null) {
	    return;
	}
	int len = hashBarLength + 20;

	xHashBar = c.createImage(len, hashBarWidth);
	yHashBar = c.createImage(hashBarWidth, len);

	Polygon poly = new Polygon();
	Graphics g = xHashBar.getGraphics();
	for (int i = 0; i < 4; i++) {
	    poly.addPoint(0,0);
	}
   	poly.ypoints[2] = hashBarWidth;
   	poly.ypoints[3] = hashBarWidth;

	for (int x = 0; x < (hashBarWidth+len); x += hashBarWidth) {
	    // draw alternate dark gray and light gray stripes.
	    if (((x/hashBarWidth)%2) == 0) {
	        g.setColor(Color.darkGray);
	    } else {
	        g.setColor(Color.lightGray);
	    }
	    poly.xpoints[0] = x;
	    poly.xpoints[1] = x + hashBarWidth;
	    poly.xpoints[2] = x;
	    poly.xpoints[3] = x - hashBarWidth;
	    g.fillPolygon(poly);
	}

	g = yHashBar.getGraphics();
   	poly.xpoints[0] = 0;
   	poly.xpoints[1] = hashBarWidth;
   	poly.xpoints[2] = hashBarWidth;
   	poly.xpoints[3] = 0;

	for (int y = 0; y < (hashBarWidth+len); y += hashBarWidth) {
	    // draw alternate dark gray and light gray stripes.
	    if (((y/hashBarWidth)%2) == 0) {
	        g.setColor(Color.darkGray);
	    } else {
	        g.setColor(Color.lightGray);
	    }
	    poly.ypoints[0] = y;
	    poly.ypoints[1] = y - hashBarWidth;
	    poly.ypoints[2] = y;
	    poly.ypoints[3] = y + hashBarWidth;
	    g.fillPolygon(poly);
	}
    }

    private static boolean isJDK(String version) {
	return System.getProperty("java.version").equals(version);
    }

    //----------------------------------------------------------------------

    private Component child;
    private Object bean;
    private String beanLabel;
    private String beanName;
    private transient boolean active;
    private transient Cursor cursor = defaultCursor;

    private final static int borderWidth = 5;
    private final static int resizeDelta = 8;    

    // Shorthands for the cursors.
    private static Cursor nwResizeCursor = Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR);
    private static Cursor neResizeCursor = Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR);
    private static Cursor swResizeCursor = Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR);
    private static Cursor seResizeCursor = Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR);
    private static Cursor moveCursor     = Cursor.getPredefinedCursor(Cursor.MOVE_CURSOR);
    private static Cursor defaultCursor  = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);

    private static Vector invisibleWrappers = new Vector();
    private transient boolean sawMouseDown;

    // Table that maps events set names to EventDescriptors.
    private transient Hashtable esdMap;
    // List of WrapperEventTargets that we fire events at.
    private Vector eventTargets;
    // List of properties that we bound into
    transient private Vector propertyTargets;

    private static int hashBarWidth = 4;
    private static int hashBarLength = 104;
    private static Image xHashBar;
    private static Image yHashBar;

    // List of this wrappers' beans' changed properties
    private transient Vector changedProperties;

    private transient boolean isFromPrototype;

    //----------------------------------------------------------------------

}

// Class to hold state on event listener hookup for which this
// Wrapper's bean is a source.

class WrapperEventTarget implements Serializable {
    static final long serialVersionUID = 4831901854891942741L;

    String eventSetName;
    Object targetBean;
    Object targetListener;
}
