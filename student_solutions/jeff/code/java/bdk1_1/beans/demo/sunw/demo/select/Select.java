
package sunw.demo.select;

/**
 * This class is a simple demo database viewer.
 * Given a URL it will go talk to a database and fill all its tables.
 * The user can then chose a table and the tool can either display metadata
 * informtion about the table or display all its rows.
 *
 * @author kgh
 */

import java.sql.*;
import java.awt.*;
import java.util.*;

public class Select extends Panel {

    public Select() {
	setLayout(null);
	setSize(200, 300);

	initialize();
    }

    private void initialize() {
	if (text != null) {
	    return;
	}
	removeAll();

        text = new java.awt.List();
        text.setFont(new Font("Courier", Font.PLAIN, 12));
	add(text);

        status = new Label("", Label.LEFT);
	add(status);

	doLayout();

	new WorkerThread(this);
    }

    // This is called from the worker thread.  This is where we actually
    // do any real work, like talking to the database.
    // Note that we have to be careful about concurrency control here so
    // thav we don't wedge the GU while we're off chatting to the database.

    void doWork() {
	for (;;) {
	    int work = getWork();	
	    if (work == DO_SELECT) {
		select();
	    }
        }
    }

    synchronized int getWork() {
	work = NO_WORK;
	while (work == NO_WORK) {
	    try {
		wait();
	    } catch (Exception ex) {
	    }
	}
	int result = work;
	return (result);
    }

    void select() {
	// System.err.println("Doing select with: " + selectString);

	text.removeAll();

	Connection con = Util.connect(url, user, password, status);
	try {
	    Statement s = con.createStatement();
	    ResultSet rs = s.executeQuery(selectString);

	    ResultSetMetaData md = rs.getMetaData();
	    int columns = md.getColumnCount();
	    int widths[] = new int[columns+1];
	    Vector contents[] = new Vector[columns+1];
	    for (int col = 1; col <= columns; col++) {
		widths[col] = md.getColumnName(col).length();
		contents[col] = new Vector();
	    }

	    int rows = 0;
	    while (rs.next() && rows < maxRows) {
		for (int col = 1; col <= columns; col++) {
		    String field = rs.getString(col);
		    if (field == null) {
			field = "";
		    }
		    if (field.length() > 80) {
			field = field.substring(0,80);
		    }
		    if (field.length() > widths[col]) {
			widths[col] = field.length();
		    }
		    contents[col].addElement(field);
		}
		rows++;
	    }

	    // Create the header string.
	    String header = "";
	    for (int col = 1; col <= columns; col++) {
	        String field = md.getColumnName(col);
		int pad = widths[col] - field.length();
		if (col != 1) {
		    pad++;
		}
		if (pad > 0) {
		    header += spaces.substring(0, pad);
		}
		header += field;
	    }
	    text.addItem(header);

	    // Add the indivbidual database rows
	    for (int row = 0; row < rows; row++) {
		String line = "";
	        for (int col = 1; col <= columns; col++) {
		    String field = (String)contents[col].elementAt(row);
		    int pad = widths[col] - field.length();
		    if (col != 1) {
			pad++;
		    }
		    if (pad > 0) {
			line += spaces.substring(0, pad);
		    }
		    line += field;
		}
   	        text.addItem(line);
	    }

	    if (text.getItemCount() >= maxRows) {
	        status.setText("Only read first " + maxRows + " rows");
	    } else {
  	        status.setText("Done.");
	    }

	} catch (SQLException sx)  {
	    System.err.println("Caught " + sx);
	    status.setText("Caught " + sx);	
	}

	Util.disconnect(con);

	doLayout();

    }

    public synchronized void doLayout() {
	initialize();
	int width = getSize().width;
	int height = getSize().height;
	text.setBounds(0, 0, width, height-40);
	status.setBounds(15, height-30, 2*width, 25);
    }

    /**
     * @deprecated provided for backward compatibility with old layout managers.
     */

    public void layout() {
	doLayout();
    }

    //------------------------------------------------------------
    // property accessors.

    public String getURL() {
	return url;
    }

    public void setURL(String url) {
	this.url = url;
    }

    public String getUser() {
	return user;
    }

    public void setUser(String user) {
	this.user = user;
    }

    public String getPassword() {
	return password;
    }

    public void setPassword(String password) {
	this.password = password;
    }

    public String getSQL() {
	return selectString;
    }

    public void setSQL(String sql) {
	selectString = sql;
    }

    public int getMaxRows() {
	return maxRows;
    }

    public void setMaxRows(int maxRows) {
	this.maxRows = maxRows;
    }


    //------------------------------------------------------------

    // Serialization support.

    private void writeObject(java.io.ObjectOutputStream s)
				      throws java.io.IOException 
    {
	// Clear the current display so we don't write all
	// the current field values.
	text.removeAll();
        s.defaultWriteObject();
    }

    private void readObject(java.io.ObjectInputStream s)
	      throws java.lang.ClassNotFoundException,
		     java.io.IOException 
    {
        s.defaultReadObject();
	initialize();
    }

    //------------------------------------------------------------

    // Event handler to re-read database state:
    public void update(java.awt.event.ActionEvent evt) {
	update();
    }

    // Public method to re-read database state.
    public synchronized void update() {
	work = DO_SELECT;
	notify();
    }

    //------------------------------------------------------------
    // GUI state:

    private transient Label status;
    private transient java.awt.List text;

    // Keep track of pending work for the worker thread:
    private final static int NO_WORK = 0;
    private final static int DO_SELECT = 3;
    private transient int work = NO_WORK;

    private final static String spaces = 
		"                                           " +
		"                                           " +
		"                                           ";

    //------------------------------------------------------------
    // Database related state:
    private String url = "jdbc:odbc:SQLSERVER";
    private String user = "guest";
    private String password = "guest";
    private String selectString = "";
    private int maxRows = 200;
}


// The worker thread just calls into the given Select()

class WorkerThread extends Thread {
    WorkerThread(Select tvarg) {
        tv = tvarg;
        this.start();
    }

    public void run() {
	tv.doWork();
    }

    private Select tv;
}

