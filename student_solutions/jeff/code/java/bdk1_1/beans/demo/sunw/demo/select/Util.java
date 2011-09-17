
package sunw.demo.select;

/**
 * Internal utility class for database viewer bean.
 */

import java.sql.*;

class Util {

    static Connection connect(String url, 
		String user, String password,
		java.awt.Label status) {
        status.setText("Connecting to " + url);
	try {
	    // Make sure there is a JDBC-ODBC driver loaded.
	    Class.forName("sun.jdbc.odbc.JdbcOdbcDriver");
	} catch (Exception ex) {
	    status.setText("Couldn't load JDBC-ODBC bridge driver");
	    System.err.println("Caught Exception : " + ex);
	    ex.printStackTrace();
	    return null;
	}

        java.util.Properties info = new java.util.Properties();
    	info.put("user", user);
    	info.put("password", password);

	// We make two attempts.  If the first attempt fails
	// we pause for a few seconds before the second attempt.
	// This works around some weird problems on wombat.

	try {
	    Connection con = DriverManager.getConnection(url, info);
	    // Success!
	    return con;
	} catch (Exception ex) {

	    // Drop through and retry.
	}

        status.setText("Connecting to " + url + " (retry)");
	try {
	    Thread.sleep(1000);

	    Connection con = DriverManager.getConnection(url, info);
	
	    // Success!
	    return con;
	} catch (SQLException sx)  {
	    status.setText("Caught " + sx);
	    System.err.println("Caught SQLException : " + sx);
	    sx.printStackTrace();
	} catch (Exception ex) {
	    status.setText("Couldn't load JDBC-ODBC bridge driver");
	    System.err.println("Caught Exception : " + ex);
	    ex.printStackTrace();
	}

	return null;
    }

    static void disconnect(Connection con) {
	if (con != null) {
	    try {
		con.close();
	    } catch (SQLException sx)  {
		System.err.println("trouble closing connection: " + sx);
	    }
	}
    }
}
