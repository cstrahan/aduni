
package sun.beanbox;

/**
 * The ToolBox is a panel that shows icons and ID strings
 * for the available JavaBeans in the current BeanBoxFrame.
 */

import java.beans.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.Vector;

// We currently keep all the contents of the ToolBox in a separate
// ToolBoxPanel object, to workaround win32 bug #4028421   KGH 1/28/97

class ToolBox extends Frame {
    private ToolBoxScrollPane pane;

    ToolBox(int x, int y) {
	super("ToolBox");

	setLayout(null);
	setBackground(Color.lightGray);
	setFont(new Font("Dialog", Font.PLAIN, 10));

	pane = new ToolBoxScrollPane(this);
	add(pane);
	pane.setLocation(0,0);

	setLocation(x, y);
	setSize(pane.getPreferredSize());
	show();
    }

    Vector getLoadedJarInfo() {
	return pane.getLoadedJarInfo();
    }

    void addBeansInJar(String jarFile) throws IOException {
	pane.addBeansInJar(jarFile);
	
	int maxNumBeans = 28;
	int maxHeight = maxNumBeans * 20 /*rowheight*/;
	Dimension dim = pane.getPreferredSize();
	
	// Pin the toolbox height to a maximium. The user will
	// have to scroll to see more than maxNumBeans.
	if (dim.height > maxHeight)
	    dim.height = maxHeight;	
	setSize(dim);	
  	validate();
    }
    
    public void doLayout() {
	super.doLayout();
	pane.setSize(getSize());
    }
}

class ToolBoxScrollPane extends ScrollPane {
    ToolBoxScrollPane(Frame frame) {
	super();

	setSize(145,300);
	setLocation(0, 0);

	tools = new ToolBoxPanel(frame);
	getVAdjustable().setUnitIncrement(20);
	add("Center", tools);
    }
	
    Vector getLoadedJarInfo() {
	return tools.getLoadedJarInfo();
    }

    void addBeansInJar(String jarFile) throws IOException {
	tools.addBeansInJar(jarFile);
    }

    public Dimension getPreferredSize() {	
	return tools.getPreferredSize();
    }

    ToolBoxPanel tools;
}

class ToolBoxPanel extends Panel implements Runnable, MouseListener {

    ToolBoxPanel(Frame frame) {
	this.frame = frame;


	setLayout(null);
	setBackground(Color.lightGray);
	setFont(new Font("Dialog", Font.PLAIN, 10));
	addMouseListener(this);

	if (! BeanBoxFrame.getQuickStart()) {
	    Vector jarNames = getJarNames();	
	    for (int i = 0; i < jarNames.size(); i++) {
	        String name = (String)jarNames.elementAt(i);
		try {
	            addBeansInJar(name);
	        } catch (Throwable th) {
		    System.err.println(name + ": jar load failed: " + th);
		    th.printStackTrace();
	        }
	    }
	}

	insertThread = new Thread(this);
	insertThread.start();
    }

    // keep it with package scope for the inner class' sake
    void addWithUniqueName(Vector v, String s) {
	if (! v.contains(s)) {
	    debug("adding "+s);
	    v.addElement(s);
	    return;
	}
	int count = 2;
	while (v.contains(s+"<"+count+">")) {
	    count += 1;
	}
	debug("adding "+s+"<"+count+">");
	v.addElement(s+"<"+count+">");
	return;
    }

    private class Helper implements DoOnBean {
	public void action(JarInfo ji, BeanInfo bi, String beanName) {
	    String label;
	    Image img = null;

	    BeanDescriptor bd = bi.getBeanDescriptor();
	    debug("Helper.action; beanName: "+beanName);
	    debug("  beanName:  "+beanName);
	    debug("  getName(): "+bd.getName());
	    debug("  getBeanClass(): "+bd.getBeanClass());

	    Class beanClass = bd.getBeanClass();
	    if (beanName.equals(beanClass.getName())) {
		label = bi.getBeanDescriptor().getDisplayName();
		img = bi.getIcon(BeanInfo.ICON_COLOR_16x16);
	    } else {
	        label = beanName;
		int ix = beanName.lastIndexOf('.');
		if (ix >= 0) {
		    label = beanName.substring(ix+1);
		}
		img = null;
	    }
	    addWithUniqueName(beanLabels, label);
	    beanNames.addElement(beanName);
	    beanIcons.addElement(img);
	    beanJars.addElement(ji);
	}
	public void error(String msg) {
	    ToolBoxPanel.this.error(msg);
	}
	public void error(String msg, Exception ex) {
	    ToolBoxPanel.this.error(msg, ex);
	}
    }

    private Helper helper = new Helper();

    /**
     * Add all the beans in a Jar file into the ToolBox
     */
    synchronized void addBeansInJar(String jarFile) throws IOException {
	JarLoader.loadJarDoOnBean(jarFile, helper);
	doLayout();
    }

    /**
     * Special action.
     *
     * Currently only used to register the BeanBox
     */
    synchronized void addBeanClass(String beanClassName) {
	Class beanClass;
	try {
	    beanClass = Class.forName(beanClassName);
	} catch (Exception ex) {
	    System.err.println("Toolbox: couldn't instantiate " +
							     beanClassName);
	    return;
	}
	BeanInfo bi;
	try {
	    bi = Introspector.getBeanInfo(beanClass);
	} catch (Exception ex) {
	    System.err.println("Toolbox: couldn't find BeanInfo for " + 
								beanClassName);
	    ex.printStackTrace();
	    return;
	}
	String label = bi.getBeanDescriptor().getDisplayName();
	Image img = bi.getIcon(BeanInfo.ICON_COLOR_16x16);
	addWithUniqueName(beanLabels, label);
	beanNames.addElement(beanClassName);
	beanIcons.addElement(img);
	beanJars.addElement(null);
	doLayout();
    }

    public Dimension getPreferredSize() {	
	if ((beanLabels != null) && beanLabels.size() != 0) {
	    return new Dimension(145, rowHeight*(beanLabels.size()+1) 
	    + frame.getInsets().bottom);
	}  else {
	    return new Dimension(145, 0); // empty toolbox
	}
    }

    public void paint(Graphics g) {
	topPad = frame.getInsets().top;
	sidePad = frame.getInsets().left;

	for (int i = 0; i < beanLabels.size(); i++) {
	    String name = (String)beanLabels.elementAt(i);
	    g.drawString(name, sidePad + 21, topPad + (rowHeight*i) + rowHeight-4);
	    Image img = (Image)beanIcons.elementAt(i);
	    if (img != null) {
	        boolean status =
		    g.drawImage(img, sidePad+2, topPad + (rowHeight*i) + 2, 16, 16, this);
	    }
	}
    }

    public void run() {
	// We run an internal thread to handle insertions, one at a time.
	for (;;) {
	    Object bean;
	    String beanLabel;
	    String beanName;
	    boolean fromPrototypeInfo = false;

	    synchronized (this) {
		// Wait for a bean to be ready for insertion.
		while (pendingBean == null) {
		    try {
			wait();
		    } catch (InterruptedException ix) {
			System.err.println("ToolBox.run: unexpected interrupt");
		    }
		}
		bean = pendingBean;
		beanLabel = pendingBeanLabel;
		beanName = pendingBeanName;
		fromPrototypeInfo = pendingFromPrototypeInfo;
	    }

	    // Figure our the current BeanBox.
	    BeanBox box = BeanBoxFrame.getCurrentBeanBox();

            // Change the cursor to indicate an "insert".
	    frame.setCursor(crosshairCursor);

	    // do the insertion.
	    box.doInsert(bean, beanLabel, beanName, false, fromPrototypeInfo);

	    // Clear any pending bean.  This will normally be the bean we
	    // just inserted, but we also want to clear any bogus insert
	    // that has been queued since we started.
	    pendingBean = null;

	    frame.setCursor(defaultCursor);
	}
    }

    /**
     * Provide information on all the loaded JAR files, regardless of origin.
     */
    public Vector getLoadedJarInfo() {
	return beanJars;
    }

    private static Vector getJarNames() {
	File cwd = new File(System.getProperty("user.dir"));
	File pwd = new File(cwd.getParent());
	File jars = new File(pwd, "jars");

	if (! jars.isDirectory()) {
	    System.err.println(jars+" is not a directory!!");
	}

	Vector result = new Vector();
	String names[];
	names = jars.list(new FileExtension(".jar"));
	for (int i=0; i<names.length; i++) {
	    result.addElement(jars.getPath() + File.separatorChar + names[i]);
	}
	names = jars.list(new FileExtension(".JAR"));
	for (int i=0; i<names.length; i++) {
	    result.addElement(jars.getPath() + File.separatorChar + names[i]);
	}

	// Do a simple bubble sort on JarNames causing
	// the order of the ToolBox contents to be consistent
	// across platforms. This makes it easier to test the BeanBox
	// using automated test scripts.	
	// Note: The order of File.list() is platform dependent.
	for (int i = result.size()-2; i >= 0; i--) {
	    for (int j = 0; j <= i; j++) {
		String s1 = (String)result.elementAt(j);
		String s2 = (String)result.elementAt(j+1);
		if (s1.compareTo(s2) > 0) {
		    Object tmp = result.elementAt(j);
		    result.setElementAt(result.elementAt(j+1), j);
		    result.setElementAt(tmp, j+1);
	    	}
	    }
	}

	return result;
    }

    //----------------------------------------------------------------------

    // Mouse listener methods for this panel.

    public void mouseClicked(MouseEvent evt) {
    }

    public void mousePressed(MouseEvent evt) {
	int row = (evt.getY() - topPad)/rowHeight;

	Object bean;

	boolean fromPrototypeInfo = false;
	String beanName = (String) beanNames.elementAt(row);
	String beanLabel = (String) beanLabels.elementAt(row);
	if (beanName.equals("sun.beanbox.BeanBox")) {
	    bean = new BeanBox();
	} else {
	    JarInfo ji = (JarInfo) beanJars.elementAt(row);
	    try {
		bean = ji.getInstance(beanName);
		fromPrototypeInfo = ji.isFromPrototype(beanName);
	    } catch (Exception ex) {
		error("instantion of a new bean failed", ex);
		return;
	    }
	}

	// Wakeup the insert thread to insert the new bean.
	synchronized (this) {
	    pendingBean = bean;
	    pendingBeanLabel = beanLabel;
	    pendingBeanName = beanName;
	    pendingFromPrototypeInfo = fromPrototypeInfo;
	    notifyAll();
	}
    }

    public void mouseReleased(MouseEvent evt) {
    }

    public void mouseEntered(MouseEvent evt) {
    }

    public void mouseExited(MouseEvent evt) {
    }

    public void mouseDragged(MouseEvent evt) {
    }

    public void mouseMoved(MouseEvent evt) {
    }

    //----------------------------------------------------------------------

    public void error(String message, Exception ex) {
	String mess = message + ":\n" + ex;
	System.err.println(message);
	ex.printStackTrace();
	// Popup an ErrorDialog with the given error message.
	new ErrorDialog(frame, mess);

    }

    public void error(String message) {
	String mess = message;
	System.err.println(message);
	// Popup an ErrorDialog with the given error message.
	new ErrorDialog(frame, mess);

    }

    static  void debug(String msg) {
	if (debug) {
	    System.err.println("ToolBox:: "+msg);
	}
    }

    Vector beanLabels = new Vector();
    Vector beanNames = new Vector();
    Vector beanIcons = new Vector();
    Vector beanJars = new Vector();
    private int topPad = 0;
    private int sidePad = 0;
    private final static int rowHeight = 20;
    private Thread insertThread = null;
    private static boolean debug = false;
    private Object pendingBean;
    private String pendingBeanLabel;
    private String pendingBeanName;
    private boolean pendingFromPrototypeInfo;
    private Frame frame;

    private static Cursor crosshairCursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);
    private static Cursor defaultCursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR);
}
