package sunw.demo.select;

/**
 * This class configures the DBViewer.
 *
 * @author kgh
 */

import java.sql.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.beans.*;

public class SelectCustomizer extends Panel implements java.beans.Customizer {

    public SelectCustomizer() {
	setLayout(null);
	Thread th = new WorkerThread();
	th.start();
    }

    public Dimension getPreferredSize() {
	return new Dimension(400, 500);
    }

    public Dimension getMinimumSize() {
	return getPreferredSize();
    }

    public void setObject(Object objectToCustomize) {
	target = (Select) objectToCustomize;

	url = target.getURL();
	user = target.getUser();
	password = target.getPassword();

	createStartupScreen();
	setVisible(true);
    }

    // Nested class for worker thread just calls our doWork method.
    class WorkerThread extends Thread {
        public void run() {
	    doWork();
        }
    }

    // This is called from the worker thread.  This is where we actually
    // do any real work, like talking to the database.
    // Note that we have to be careful about concurrency control here so
    // thav we don't wedge the GUI while we're off chatting to the database.

    void doWork() {
	for (;;) {
	    int work = getWork();

	    // Open a database connection.
	    con = Util.connect(url, user, password, status);
	    if (con == null) {
		continue;
	    }

	    if (work == DO_USER_TABLES) {
	        getTables(false);
	    } else if (work == DO_ALL_TABLES) {
	        getTables(true);
	    } else if (work == DO_VIEW) {
		getInfo(tableName);
	    }

	    // Close the database connection.
	    Util.disconnect(con);
        }
    }

    private synchronized int getWork() {
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

    synchronized void setWork(int newWork) {
	work = newWork;
	notify();
    }

    private void getTables(boolean showSystemTables) {
	try {
	    setStatus("Opened connection OK, searching for tables...");

	    DatabaseMetaData md = con.getMetaData();
	    String types[];
	    if (showSystemTables) {
  	        types = new String[2];
	        types[0] = "TABLE";
	        types[1] = "SYSTEM TABLE";
	    } else {
  	        types = new String[2];
	        types[0] = "TABLE";
	    }
	    ResultSet rs = md.getTables(null, "%", "%", types);

	    tableNames = new Vector();
	    int count = 0;
	    while (rs.next()) {
	        String name = rs.getString(2) + "." + rs.getString(3);
		tableNames.addElement(name);
		count++;
	    }
	    rs.close();

	    setStatus("Got " + count + " tables");

	    createTableScreen();

	} catch (SQLException sx)  {
	    setStatus(sx);
	    System.err.println("Caught SQLException : " + sx);
	    sx.printStackTrace();
	} catch (Exception ex) {
	    setStatus("Couldn't load JDBC-ODBC bridge driver");
	    System.err.println("Caught Exception : " + ex);
	    ex.printStackTrace();
	}
    }

    public void getInfo(String table) {
	// Dsiplay information on the contents of a table:
	createInfoScreen(table);
	columnNames = new Vector();
	columnTypes = new Vector();
	try {
	    int index = table.indexOf('.');
	    String schema = table.substring(0, index);
	    String name = table.substring(index+1, table.length());

	    DatabaseMetaData md = con.getMetaData();
	    ResultSet rs = md.getColumns(null, schema, name, "%");
	    while (rs.next()) {
		String columnName = rs.getString(4);
		columnNames.addElement(columnName);
		int type = rs.getShort(5);
		int width = rs.getShort(7);
		int scale = rs.getShort(9);
		String typeName;
		switch (type) {
	 	  case Types.BIT:
		    typeName = "BIT";
		    break;
	 	  case Types.TINYINT:
		    typeName = "TINYINT";
		    break;
	 	  case Types.SMALLINT:
		    typeName = "SMALLINT";
		    break;
	 	  case Types.INTEGER:
		    typeName = "INTEGER";
		    break;
	 	  case Types.BIGINT:
		    typeName = "BIGINT";
		    break;
	 	  case Types.FLOAT:
		    typeName = "FLOAT";
		    break;
	 	  case Types.DOUBLE:
		    typeName = "DOUBLE";
		    break;
	 	  case Types.REAL:
		    typeName = "REAL";
		    break;
	 	  case Types.NUMERIC:
		    typeName = "NUMERIC(" + width + "," + scale + ")";
		    break;
	 	  case Types.DECIMAL:
		    typeName = "DECIMAL(" + width + "," + scale + ")";
		    break;
	 	  case Types.CHAR:
		    typeName = "CHAR(" + width + ")";
		    break;
	 	  case Types.VARCHAR:
		    typeName = "CHAR(" + width + ")";
		    break;
	 	  case Types.LONGVARCHAR:
		    typeName = "LONGVARCHAR";
		    break;
	 	  case Types.BINARY:
		    typeName = "BINARY(" + width + ")";
		    break;
	 	  case Types.VARBINARY:
		    typeName = "VARBINARY(" + width + ")";
		    break;
	 	  case Types.LONGVARBINARY:
		    typeName = "LONGVARBINARY";
		    break;
	 	  case Types.DATE:
		    typeName = "DATE";
		    break;
	 	  case Types.TIME:
		    typeName = "TIME";
		    break;
	 	  case Types.TIMESTAMP:
		    typeName = "TIMESTAMP";
		    break;
	 	  case Types.OTHER:
		    typeName = "OTHER";
		    break;
		  default:
		    typeName = "???";
		    break;
		}
		columnTypes.addElement(typeName);
	    }
	    rs.close();
	    finishInfoScreen();

	} catch (SQLException sx)  {
	    setStatus(sx);
	}
    }

    synchronized void resetScreen() {
	// Do the basic screen setup that's common to all our environments.
	removeAll();

	int width = getSize().width;
	int height = getSize().height;

	buttonOffset = 10;

	status = new Label("", Label.LEFT);
	add(status);
	status.setBounds(15, height-30, 2*width, 25);
    }

    synchronized void addButton(Button b) {
	int height = getSize().height;
	add(b);
	b.setBounds(buttonOffset, height-65, 80, 30);
	buttonOffset += 80;
    }

    synchronized void createStartupScreen() {

	// Set up the initial screen where we prompt the user for a
	// database URL, account, and password.

	resetScreen();
	int width = getSize().width;

	Label l1 = new Label("Welcome to the Select Customizer.", Label.CENTER);
	add(l1);
	l1.setFont(new Font("Dialog", Font.PLAIN, 16));
	l1.setBounds(10,100,width-20,50);

	Label t1 = new Label("JDBC URL:", Label.RIGHT);
	add(t1);
	t1.setBounds(20, 250, 70, 30);
	urlField = new TextField(url, 40);
	urlField.addKeyListener(new UrlListener());
	add(urlField);
	urlField.setBounds(100, 250, 280, 30);

	Label t2 = new Label("User:", Label.RIGHT);
	add(t2);
	t2.setBounds(20, 300, 70, 30);
	userField = new TextField(user, 40);
	userField.addKeyListener(new UserListener());
	add(userField);
	userField.setBounds(100, 300, 280, 30);

	Label t3 = new Label("Password:", Label.RIGHT);
	add(t3);
	t3.setBounds(20, 350, 70, 30);
	passwordField = new TextField(password, 40);
	passwordField.setEchoChar('*');
	passwordField.addKeyListener(new PasswordListener());
	add(passwordField);
	passwordField.setBounds(100, 350, 280, 30);

	setStatus("Enter login info and push \"user tables\" or \"all tables\"");

	Button user = new Button("User Tables");
	user.addActionListener(new UserTablesListener());
	addButton(user);

	Button all = new Button("All Tables");
	all.addActionListener(new AllTablesListener());
	addButton(all);

    }

    synchronized void createTableScreen() {

	// Set up the screen where we report on all known tables.
	resetScreen();
	int width = getSize().width;
	int height = getSize().height;

	String summary = "Database \"" + url + "\" has " + tableNames.size() + " tables.";
	Label l1 = new Label(summary, Label.LEFT);
	add(l1);
	l1.setBounds(30,10,width-20,25);

	Button back = new Button("Back");
	back.addActionListener(new BackToStartListener());
	addButton(back);

	tableList = new java.awt.List();
	tableList.addItemListener(new TableListener());
	add(tableList);
	for (int i = 0; i < tableNames.size(); i++) {
	    String name = (String)tableNames.elementAt(i);
	    tableList.addItem(name);
	}
	tableList.setBounds(30, 40, 240, height-110);

	setStatus("Please chose a table");

    }

    synchronized void createInfoScreen(String table) {

	// Set up the screen where we describe a table.
	resetScreen();
	int width = getSize().width;
	int height = getSize().height;
    
	Label l1 = new Label("Database:  " + url, Label.LEFT);
	add(l1);
	l1.setBounds(30,35,width-40,20);

	Label l2 = new Label("Table:    " + table, Label.LEFT);
	add(l2);
	l2.setBounds(30,55,width-20,20);

	Button back = new Button("Back");
	back.addActionListener(new BackToTableListener());
	addButton(back);

	setStatus("Loading table info...");
    }

    synchronized void finishInfoScreen() {
	int width = getSize().width;
	int height = getSize().height;

	Label l3 = new Label("Number of columns:  " + columnNames.size(), Label.LEFT);
	add(l3);
	l3.setBounds(30,75,width-20,20);

	columnList = new java.awt.List();
	columnList.setFont(new Font("Courier", Font.PLAIN, 12));
	columnList.setMultipleMode(true);
	columnList.addItemListener(new ColumnListener());
	add(columnList);
	for (int i = 0; i < columnNames.size(); i++) {
	    String name = (String)columnNames.elementAt(i);
	    String type = (String)columnTypes.elementAt(i);
	    int pad = 22 - name.length();
	    String entry = name;
	    if (pad > 0) {
		entry += spaces.substring(0,pad);
	    }
	    entry += type;
	    columnList.addItem(entry);
	}
	columnList.setBounds(30, 120, 260, height-200);
	setStatus("Choose the columns you want in the SQL SELECT.");
    }
    
    synchronized void setStatus(String mess) {
	status.setText(mess);
    }

    synchronized void setStatus(SQLException sx) {
	System.err.println("Caught " + sx);
	setStatus("" + sx);
    }

    void updateSelect() {
	int len = columnList.getItemCount();
	String columns = null;
	for (int i = 0; i < len; i++) {
	    if (columnList.isIndexSelected(i)) {
	        String name = (String)columnNames.elementAt(i);
		if (columns == null) {
		    columns = name;
		} else {
		    columns = columns + ", " + name;
	     	}
	    }
	}
	if (columns == null) {
	    selectString = "";
	} else {
	    selectString = "SELECT " + columns + " FROM " + tableName;
	}
	System.err.println(selectString);
	target.setSQL(selectString);
	support.firePropertyChange("SQL", null, selectString);
	target.update();
    }


    static void println(String mess) {
	System.out.println(mess);
    }

    static void print(String mess) {
	System.out.print(mess);
    }

    //----------------------------------------------------------------------

    // Nested classes for various GUI event listeners.
    //
    // Now that in JDK 1.1 these nested classes can only access public and
    // package-private methods/fields of the containing class adn can't
    // access private fields.
    //
    // So we've been careful to make sure that the fields they need are
    // at least package-private.

    class UrlListener extends KeyAdapter {
	public void keyReleased(KeyEvent evt) {
	    // Someone edited the "URL" string.
	    url = urlField.getText();
	    target.setURL(url);
	    support.firePropertyChange("URL", null, url);
	}
    }

    class UserListener extends KeyAdapter {
	public void keyReleased(KeyEvent evt) {
	    // Someone edited the "user" string.
            user = userField.getText();
	    target.setUser(user);
	    support.firePropertyChange("user", null, user);
	}
    }

    class PasswordListener extends KeyAdapter {
	public void keyReleased(KeyEvent evt) {
	    // Someone edited the "password" string.
            password = passwordField.getText();
	    target.setPassword(password);
	    support.firePropertyChange("password", null, password);
	}
    }

    class ColumnListener implements ItemListener {
	public void itemStateChanged(ItemEvent evt) {
	     // Someone selected a column in our column view.
	    updateSelect();
	}
    }

    class TableListener implements ItemListener {
	public void itemStateChanged(ItemEvent evt) {
	     // Someone selected a table in our table view.
	    tableName = tableList.getSelectedItem();
	    setWork(DO_VIEW);
	}
    }

    class UserTablesListener implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    setWork(DO_USER_TABLES);
        }
    }

    class AllTablesListener implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    setWork(DO_ALL_TABLES);
        }
    }

    class BackToStartListener implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    createStartupScreen();
        }
    }

    class BackToTableListener implements ActionListener {
	public void actionPerformed(ActionEvent evt) {
	    createTableScreen();
        }
    }

    //----------------------------------------------------------------------

    public void addPropertyChangeListener(PropertyChangeListener l) {
	support.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
	support.removePropertyChangeListener(l);
    }

    PropertyChangeSupport support = new PropertyChangeSupport(this);

    //------------------------------------------------------------

    // GUI state:
    TextField urlField;
    TextField userField;
    TextField passwordField;
    Label status = new Label("");
    int buttonOffset;

    // Keep track of pending work for the worker thread:
    final static int NO_WORK = 0;
    final static int DO_USER_TABLES = 1;
    final static int DO_ALL_TABLES = 2;
    final static int DO_VIEW = 3;
    int work = NO_WORK;

    String spaces = "                                       " +
		"                                           " +
		"                                           ";

    //------------------------------------------------------------
    // Database related state:
    Connection con;
    String url;
    String user;
    String password;
    Vector tableNames;
    java.awt.List tableList;
    java.awt.List columnList;
    String tableName;
    String selectString;
    Vector columnNames;
    Vector columnTypes;
    //------------------------------------------------------------

    // Object that we are customizing.
    Select target;
}



