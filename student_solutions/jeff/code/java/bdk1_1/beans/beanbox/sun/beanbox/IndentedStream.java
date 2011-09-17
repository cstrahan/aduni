package sun.beanbox;

import java.io.*;

/**
 * This is a utility class for the generation of Source Files.
 */

public class IndentedStream {
    
    public IndentedStream(PrintWriter s, String space) {
	currentStream = s;
	indentSpace = space;
    }

    public IndentedStream(PrintWriter s) {
	this(s, "    ");
    }

    public void close() {
        currentStream.close();
    }

    public void o() {
	indentLevel--;
    }

    public void i() {
    	indentLevel++;
    }

    public void ip(String s) {
    	i();
	pp(s);
    }

    public void ip() {
    	i();
	pp();
    }

    public void op(String s) {
    	o();
	pp(s);
    }

    public void op() {
    	o();
	pp();
    }

    public void pp() {
	currentStream.println(); 
    }

    public void pp(String s) {
	for (int i=0; i<indentLevel; i++) {
	    currentStream.print(indentSpace);
	}
	currentStream.println(s);
    }

    public void pp0(String s) {
	for (int i=0; i<indentLevel; i++) {
	    currentStream.print(indentSpace);
	}
	currentStream.print(s);
    }

    public void pn(String s) {
    	currentStream.println(s);
    }

    public void pn0(String s) {
        currentStream.print(s);
    }

    // Private fields for indented streams...

    private int indentLevel;	// indentation level
    private String indentSpace; // space
    private PrintWriter currentStream; // for generating code
}
