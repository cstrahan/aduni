
/**
 * Pop up a MakeApplet dialog
 */

package sun.beanbox;

import java.io.*;
import java.awt.*;
import java.awt.event.*;

public class MakeAppDlg extends Dialog implements Runnable {

    private TextField appletNameField;
    private TextField appletJarField;
    private boolean dismissed = false;
    private Frame frame;
    private BeanBox beanbox;

    private static String defaultJarName
        = "tmp"+File.separator+"myApplet"+File.separator+"myApplet.jar";
    private static String defaultAppletName
        = beanNameFromJarName(defaultJarName);

    public MakeAppDlg(Frame frame, BeanBox bb) {
	super(frame, "Make an Applet", false);
    	new WindowCloser(this);
	this.frame = frame;
	this.beanbox = bb;

	GridBagLayout gb = new GridBagLayout();
	this.setLayout(gb);

	GridBagConstraints middle = new GridBagConstraints();
	middle.anchor = GridBagConstraints.WEST;
	GridBagConstraints last = new GridBagConstraints();
	last.gridwidth = GridBagConstraints.REMAINDER;
	last.anchor = GridBagConstraints.WEST;
	GridBagConstraints only = new GridBagConstraints();
	only.gridwidth = GridBagConstraints.REMAINDER;

	Label l, ll;
	TextField f;
	Button b;

	l = new Label("Select a JAR file where to package an Applet.");
	gb.setConstraints(l, last);
	add(l);

	l = new Label("Jar File: ");
	appletJarField = f = new TextField(defaultJarName, 30);
	b = new Button("Choose JAR File...");
	gb.setConstraints(l, middle);
	gb.setConstraints(f, middle);
	gb.setConstraints(b, last);
	b.addActionListener(new ChooseAdaptor());
	add(l);
	add(f);
	add(b);

	l = new Label("Applet Class: ");
	appletNameField = f = new TextField(defaultAppletName, 30);
	gb.setConstraints(l, middle);
	gb.setConstraints(f, last);
	add(l);
	add(f);
	
	Button bOK, bCancel, bHelp;

	Panel p = new Panel();
	bOK= new Button("OK");
	bCancel = new Button("Cancel");
	bHelp = new Button("Help");
	p.add(bOK);
	p.add(bCancel);
	p.add(bHelp);
	gb.setConstraints(p, only);
	add(p);

	bCancel.addActionListener(new CancelAdaptor());
	bOK.addActionListener(new OKAdaptor());
	bHelp.addActionListener(new HelpAdaptor());

        int x = frame.getLocation().x + 30;
        int y = frame.getLocation().y + 100;
	pack();
	Dimension d = getPreferredSize();
        setBounds(x, y, d.width, d.height);
        show();
    }

    void chooseFile() {
	FileDialog fd = new FileDialog(frame, "Choose JAR File",
				       FileDialog.SAVE);

	File file = new File(defaultJarName);
	File dir = new File(file.getParent());

	if (! dir.exists()) {
	    dir.mkdirs();
	}

	fd.setDirectory(dir.getPath());
	fd.setFile(file.getName());
	fd.setFilenameFilter(new FileExtension("jar"));

	fd.show();

	String fname = fd.getFile();
	if (fname == null) {
	    return;
	}
	if (! fname.endsWith(".jar")) {
	    new ErrorDialog(frame, "JAR file should end in .jar");
	    return;
	}

	File f = new File(fd.getDirectory(), fname);
	String s = f.getPath();

	appletJarField.setText(s);
	appletNameField.setText(beanNameFromJarName(s));
    }

    /**
     * Auxiliary method so user most likely needs not type the BeanName
     */
    private static String capitalize(String name) {
	if (name == null || name.length() == 0) {
	    return name;
	}
	char chars[] = name.toCharArray();
	chars[0] = Character.toUpperCase(chars[0]);
	return new String(chars);
    }

    private static String beanNameFromJarName(String fname) {
	if (! fname.endsWith(".jar")) {
	    return null;
	}
	int i = fname.lastIndexOf(File.separator);
	String name = fname.substring(i+1, fname.length()-4);
	return capitalize(name);
    }

    /**
     * validation of input
     */

    boolean validateInput() {
	String s = appletJarField.getText();
	if (! s.endsWith(".jar")) {
	    return false;
	}
	// other tests would go here
	return true;
    }

    /**
     * The applet name selected by user,
     * or null if cancelled
     */
    private String getAppletName() {
	return appletNameField.getText();
    }

    /**
     * The directory selected by user,
     */
    private String getAppletDirectory() {
	File f = new File(appletJarField.getText());
	return f.getParent();
    }

    /**
     * The name of the desired JAR file.  This is relative to AppletDirectory
     */
    private String getJarName() {
	File f = new File(appletJarField.getText());
	String s = f.getName();
	return s.substring(0, s.length()-4);
    }

    // Adaptors;
    // one could also discriminate on evt.getSource()
    
    class ChooseAdaptor implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    chooseFile();
	}
    }
    class CancelAdaptor implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    dismissed = true;
	    dispose();
	}
    }
    class OKAdaptor implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    if (! validateInput() ) {
		new ErrorDialog(frame, "invalid JAR filename");
		return;
	    }
	    Thread th = new Thread(MakeAppDlg.this);
	    th.start();
	}
    }

    /**
     * Update the status message
     */
    void updateMessage(String message) {
	// System.err.println(message);
	status2.setText(message);
	repaint();
    }
    private Label status2;

    /**
     * Run is called in a separate thread to actually complete the
     * generation of the applet data, including the compilation
     */
    public void run() {
	removeAll();
	setLayout(null);
	Label status = new Label("Generating and compiling "+getAppletName()+" Files");

	add(status);
	status.setBounds(20, getSize().height/2 - 10, getSize().width-30, 15);
	status2 = new Label();
	add(status2);
	status2.setBounds(20, getSize().height/2 + 10, getSize().width-30, 15);
	updateMessage("Start...");
	repaint();
	
	AppletGenerator.generate(frame, beanbox, this,
				 getAppletDirectory(),
				 getAppletName(),
				 getJarName());
	dispose();
    }
    class HelpAdaptor implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    new MakeAppletHelpDialog(frame);
	}
    }
}

class MakeAppletHelpDialog extends MessageDialog {
    private final static String message
        = "To generate an Applet, you must specify a JAR file\n"
        + "with a name that ends with \".jar\", as in MyApplet.jar.\n"
        + "\n"
        + "The default name of the Applet is inferred from the name\n"
        + "of the JAR file, MyApplet in the example above\n"
        + "\n"
        + "This operation will generate a JAR file (MyApplet.jar), a readme\n"
        + "file (MyApplet_readme), and an HTML file (MyApplet.html) that can\n"
        + "be used to test the Applet in a JDK1.1-compliant browser.\n"
        + "Additional files will be generated into a subdirectory with the name\n"
        + "MyApplet_files.\n" 
        + "\n"
        + "The applet will work with appletviewer and HotJava but there are some\n"
        + "restrictions in the current versions of other browsers."
        + "\n"
        + "For additional details check the generated readme file.\n"
        + "\n";


    public MakeAppletHelpDialog(Frame frame) {
	super(frame, "Help for MakeApplet", message, true);
    }
}
