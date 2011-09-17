
package sunw.demo.select;

import java.sql.*;

class Populate {

    public static void main(String argv[]) {

        connect();

	createTables();

	addSparcs();

        try {
            Statement s = getStatement();
    	    ResultSet rs = s.executeQuery("SELECT COUNT(*) FROM PIPELINES");
	    rs.next();
	    System.err.println("PIPELINE COUNT = " + rs.getInt(1));
    	    rs = s.executeQuery("SELECT COUNT(*) FROM CHIPS");
	    rs.next();
	    System.err.println("CHIP COUNT = " + rs.getInt(1));
    	    rs = s.executeQuery("SELECT COUNT(*) FROM BOXES");
	    rs.next();
	    System.err.println("BOX COUNT = " + rs.getInt(1));
	} catch (SQLException sx) {
	    report(sx);
	    throw new Error("" + sx);
	}
    }

    static void createTables() {
	try {
	    Statement s = getStatement();

	    try {
		s.execute("DROP TABLE BOXES");
	    } catch (Exception ex) {	
	    }
	    try {
		s.execute("DROP TABLE CHIPS");
	    } catch (Exception ex) {
	    }
	    try {
		s.execute("DROP TABLE PIPELINES");
	    } catch (Exception ex) {
	    }

	    s.execute("CREATE TABLE PIPELINES ( " +
			"YEAR CHAR(4), " +
			"NAME VARCHAR(30), " +
			"STAGES INTEGER," +
			"PRIMARY KEY (NAME)" +
			")");

	    s.execute("CREATE TABLE CHIPS ( " +
			"YEAR CHAR(4), " +
			"NAME VARCHAR(30), " +
			"NICKNAME VARCHAR(30), " +
			"PIPELINE VARCHAR(30), " + 
		        "LOW_MHZ VARCHAR(4)," +
		        "HIGH_MHZ VARCHAR(4)," +
			"VARIANT VARCHAR(5)," +
			"PRIMARY KEY (NAME), " + 
			"FOREIGN KEY (PIPELINE) REFERENCES PIPELINES" + 
			")");

	    s.execute("CREATE TABLE BOXES ( " +
			"QUARTER CHAR(4), " +
			"VENDOR VARCHAR(50), " +
			"MODEL VARCHAR(100), " +
			"ARCHITECTURE VARCHAR(30), " +
			"CHIP VARCHAR(30), " +
			"MHZ VARCHAR(4), " +
			"SPEC89 VARCHAR(8), " +
			"SPECINT92 VARCHAR(8), " +
			"SPECFP92 VARCHAR(8), " +
			"SPECINT95 VARCHAR(8), " +
			"SPECFP95 VARCHAR(8), " +
			"PRICE VARCHAR(10), " +
			"CONFIGURATION VARCHAR(100), " +
			"PRIMARY KEY (MODEL), " + 
			"FOREIGN KEY (CHIP) REFERENCES CHIPS" + 
			")");

	} catch (SQLException sx) {
	    report(sx);
	    throw new Error("" + sx);
	}
    }

    static String names;
    static String values;
    static String updates;

    private static void start() {
	names = "";
	values = "";
        updates = "";
    }

    private static void push(String name, String value) {
        if (names.length() > 0) {
	    names += ", ";
	}
	names += name;
        if (values.length() > 0) {
	    values += ", ";
	}
	values += "'" + value + "'";
	if (updates.length() > 0) {
	    updates += ", ";
	}
	updates += name + " = '" + value + "'";
    }


    static void insertBox(String model) {
	try {
	    con.setAutoCommit(false);

	    // First check if there is already a corresponding column in the table
	    Statement s = getStatement();
	    String query = "SELECT MODEL FROM BOXES WHERE MODEL = '" + model + "'";

	    ResultSet rs = s.executeQuery(query);
	    int count = 0;
	    while (rs.next()) {
		count++;
	    }
	    s.close();

	    String sql;
	    if (count == 0) {
	        sql = "INSERT INTO BOXES ( " + names + " ) VALUES ( " + values + " )";
	    } else {
	        sql = "UPDATE BOXES SET " + updates + " WHERE  MODEL = '" + model + "'";
			    
	    }

	    s = getStatement();
	    s.execute(sql);
	    s.close();

	    con.commit();

	} catch (SQLException sx) {
	    report(sx);
	    throw new Error("" + sx);
	}
    }


    static void box(String q, String model, String cpu, int mhz, int int92, int fp92, int dollars, String config) {
	String quarter = q.substring(2,4) + q.substring(0,2);

	start();
	push("QUARTER", quarter);
	push("VENDOR", "Sun");
	push("ARCHITECTURE", "Sparc");
	push("MODEL", model);
	push("CHIP", cpu);
	if (mhz != 0) {
	    push("MHZ", "" + mhz);
	}
	if (int92 != 0) {
	    push("SPECINT92", "" + int92);
	}
	if (fp92 != 0) {
	    push("SPECFP92", "" + fp92);
	}
	if (dollars != 0) {
	    push("PRICE", "" + dollars);
	}
	if (config != null && !config.equals("")) {
	    push("CONFIGURATION", config);
	}
	insertBox(model);
    }


    static void box(String q, String model, String cpu, String mhz, String spec89, String dollars) {

	String quarter = q.substring(2,4) + q.substring(0,2);

	start();
	push("QUARTER", quarter);
	push("VENDOR", "Sun");
	push("ARCHITECTURE", "Sparc");
	push("MODEL", model);
	push("CHIP", cpu);
        push("MHZ", mhz);
	push("SPEC89", spec89);
        push("PRICE", dollars);
	insertBox(model);
    }

    static void chip(int year, String name, String nickname, String pipeline, int lhz, int hhz, String version) {
	start();
	push("YEAR", "" + year);
	push("NAME", name);
	push("NICKNAME", nickname);
	push("PIPELINE", pipeline);
	if (lhz != 0) {
	   push("LOW_MHZ", "" + lhz);
	}
	if (hhz != 0) {
	    push("HIGH_MHZ", "" + hhz);
	}
	push("VARIANT", version);
	
	try {
	    con.setAutoCommit(false);

	    // First check if there is already a corresponding column in the table
	    Statement s = getStatement();
	    String query = "SELECT NAME FROM CHIPS WHERE NAME = '" + name + "'";

	    ResultSet rs = s.executeQuery(query);
	    int count = 0;
	    while (rs.next()) {
		count++;
	    }
	    s.close();

	    String sql;
	    if (count == 0) {
	        sql = "INSERT INTO CHIPS ( " + names + " ) VALUES ( " + values + " )";
	    } else {
	        sql = "UPDATE CHIPS SET " + updates + " WHERE NAME = '" + name + "'";
			    
	    }

	    s = getStatement();
	    s.execute(sql);
	    s.close();

	    con.commit();

	} catch (SQLException sx) {
	    report(sx);
	    throw new Error("" + sx);
	}
    }

    static void pipeline(int year, String name, int stages) {
	start();
	push("YEAR", "" + year);
	push("NAME", name);
	if (stages != 0) {
	    push("STAGES", "" + stages);
	}
	
	try {
	    con.setAutoCommit(false);

	    // First check if there is already a corresponding column in the table
	    Statement s = getStatement();
	    String query = "SELECT NAME FROM PIPELINES WHERE NAME = '" + name + "'";

	    ResultSet rs = s.executeQuery(query);
	    int count = 0;
	    while (rs.next()) {
		count++;
	    }
	    s.close();

	    String sql;
	    if (count == 0) {
	        sql = "INSERT INTO PIPELINES ( " + names + " ) VALUES ( " + values + " )";
	    } else {
	        sql = "UPDATE PIPELINES SET " + updates + " WHERE NAME = '" + name + "'";
			    
	    }

	    s = getStatement();
	    s.execute(sql);
	    s.close();

	    con.commit();

	} catch (SQLException sx) {
	    report(sx);
	    throw new Error("" + sx);
	}
	
    }

    static void addSparcs() {

	pipeline(1995, "Spitfire", 0);
	pipeline(1994, "HyperSparc", 0);
	pipeline(1992, "Viking", 0);
	pipeline(1992, "Tsunami", 0);
	pipeline(1987, "Original", 0);

	chip(1995, "UltraSparc", "Spitfire", "Spitfire", 143, 200, "V9");
	chip(1996, "UltraSparc-II", "Blackbird", "Spitfire", 0, 0, "V9");
	chip(1994, "HyperSparc", "", "HyperSparc", 75, 150, "V8");
	chip(1992, "SuperSparc", "Viking", "Viking", 30, 60, "V8");
	chip(1995, "SuperSparc-II", "Voyager", "Viking", 75, 85, "V8");
	chip(1992, "MicroSparc", "Tsunami", "Tsunami", 50, 50, "V8");
	chip(1994, "MicroSparc-II", "Swift", "Tsunami", 60, 110, "V8");
	chip(1989, "Cypress", "", "Original", 25, 40, "V7");
	chip(1987, "Fujitsu", "", "Original", 17, 25, "V7");


	box("Q295", "SPARCstation 4/110",	"MicroSparc-II", 	110,	 65,  53,    4000, 	"15\"C 32M 1GD");
	box("Q295", "SPARCstation 5/110", 	"MicroSparc-II", 	110,     79,  65,   10000, 	"17\"C 32M 1GD");
	box("Q194", "SPARCstation Voyager","MicroSparc-II",  	 60,	 43,  37,    9000,	"");
	box("Q294", "SPARCstation 20/50", 	"SuperSparc",  		 50,     69,  78,   12000, 	"");
	box("Q294", "SPARCstation 20/51", 	"SuperSparc",  		 50,     74,  85,   0, 		"");
	box("Q294", "SPARCstation 20/61", 	"SuperSparc", 		 60,     89, 103,   16000, 	"");
	box("Q195", "SPARCstation 20/71", 	"SuperSparc-II",  	 75,    126, 121,   18000, 	"17\"C 32M 1GD");
	box("Q295", "SPARCstation 20/HS21","HyperSparc", 		125,   	131, 153,   19000,	"17\"C 32M 1GD");
	box("Q495", "SPARCstation 20/151", "HyperSparc",		150,	169, 208,   19000,	"20\"C 32M 1GD");
	box("Q495", "Ultra 1/140", 	"UltraSparc",	        143, 	215, 303,   17000, 	"17\"C 32M 1GD");
	box("Q495", "Ultra 1/170", 	"UltraSparc",    	167,    252, 351,   26000,  	"20\"C 64M 2GD");
	box("Q196", "Ultra 2/2200", "UltraSparc", 		200,    332, 505,   58000, 	"20\"C 256M 4GD");

	box("??87", "Sun 4/280", "Fujitsu", "17", "8.1", "");
	box("Q289", "SPARCstation 1", "Fujitsu", "20", "8.4", "9000");
	box("Q289", "SPARCstation 330", "Cypress", "25", "11.8", "29000");
	box("Q290", "SPARCstation 1+", "Fujitsu", "25", "11.8", "9000");
	box("Q290", "SPARCstation SLC", "Fujitsu", "20", "9.8", "5000");
	box("Q390", "SPARCstation IPC", "Fujitsu", "25", "13.5", "9000");
	box("Q490", "SPARCstation 2", "Cypress", "40", "25.0", "15000");
	box("Q391", "SPARCstation ELC", "Cypress", "33", "20.1", "4000");
	box("Q390", "SPARCstation IPC", "Fujitsu", "25", "13.5", "6000");
	box("Q391", "SPARCstation IPX", "Cypress", "40", "24.2", "10000");

	box("Q390", "SPARCstation IPC", "Cypress", 25, 14, 11, 6000, "");
	box("Q391", "SPARCstation ELC", "Cypress", 33, 18, 18, 4000, "");
	box("Q391", "SPARCstation IPX", "Cypress", 40, 22, 21, 10000, "");
	box("Q392", "SPARCstation 10/41", "UltraSparc", 40, 53, 65, 23000, "");
	box("Q490", "SPARCstation 2", "Cypress", 40, 22, 23, 15000, "");
	box("Q392", "SPARCstation 10/20", "UltraSparc", 33, 41, 44, 16000, "");
	box("Q492", "SPARCstation LX", "MicroSparc", 50, 26, 21, 8000, "");
	box("Q392", "SPARCstation 10/30", "UltraSparc", 36, 44, 53, 16000, "");
	box("Q392", "SPARCstation 10/40", "UltraSparc", 40, 53, 65, 21000, "");
	box("Q293", "SPARCstation 10/51", "UltraSparc", 50, 65, 80, 28000, "");
	box("Q294", "SPARCstation 5", "MicroSparc-II", 70, 57, 47, 4000, "15\"C 16M 500MD");
	box("Q494", "SPARCstation 20/HS11", "HyperSparc", 100, 104, 128, 19000, "17\"C 32M 1GD");
	box("Q195", "SPARCstation 4", "MicroSparc-II", 70, 60, 47, 4000, "15\"C 16M 535MD");
	box("Q294", "SPARCstation 5", "MicroSparc-II", 85, 64, 56, 10000, "17\"C 32M 1GD");
	box("Q492", "SPARCclassic", "MicroSparc", 50, 26, 21, 3000, "15\"C 16M 200MD");
	box("Q295", "SPARCstation 4/85", "MicroSparc-II", 85, 65, 53, 4000, "15\"C 16M 535MD");

    }

    public static Connection connect() {
        // DriverManager.setLogStream(System.err);

	if (con != null) {
	    return (con);
	}

	try {
	    // Make sure there is a JDBC-ODBC driver loaded.
	    Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
	} catch (Exception ex) {
	    System.err.println("Couldn't load JDBC-ODBC bridge driver");
	    System.err.println("Caught Exception : " + ex);
	    ex.printStackTrace();
	    return null;
	}

	String url = "jdbc:odbc:SQLSERVER";

	println("Trying to connect to URL " + url);
	try {

	    java.util.Properties info = new java.util.Properties();
	    info.put("user", "guest");
	    info.put("password", "guest");
	    con = DriverManager.getConnection(url, info);

	    println("Got connection " + con);
	    return (con);
	} catch (SQLException ex) {
	    report(ex);
	    throw new Error("" + ex);
	} catch (Exception ex) {
	    throw new Error("" + ex);
	}
    }

    public static Statement getStatement() {

	try {
	    return (con.createStatement());
	} catch (SQLException ex) {
	    report(ex);
	    throw new Error("" + ex);
	}
    }

    public static void report (SQLException sx) {
        println("Caught SQLsxception " + sx);
	println("   reason = " + sx.getMessage());
	println("   vendor code = " + sx.getErrorCode());
	println("   SQL state = " + sx.getSQLState());
	sx.printStackTrace();
    }

    public static void println(String mess) {
	System.err.println(mess);
    }


    private static Connection con = null;
}
