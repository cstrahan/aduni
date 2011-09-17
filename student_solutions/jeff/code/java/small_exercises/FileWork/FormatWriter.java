import java.io.*;

public class FormatWriter extends PrintWriter
{
    private int width = 10;

    // Basic constructor for a default field width
    public FormatWriter(Writer output)
    {
	super(output);
    }

    // Constructor with a specified field width
    public FormatWriter(Writer output, int width)
    {
	super(output);  // Call PrintStream constructor
	this.width = width; // Store the field width
    }

    // Constructor with autoflush option
    public FormatWriter(Writer output, boolean autoflush)
    {
	super(output, autoflush);
    }

    // Constructor with a specified field width and autoflush option
    public FormatWriter(Writer output, boolean autoflush, int width)
    {
	super(output, autoflush);
	this.width = width;
    }

    // Helper method for output
    private void output(String str)
    {
	int blanks = width - str.length(); // Number of blanks needed

	// If the length is less than the width, add blanks to the start
	for(int i = 0; i < blanks; i++)
	    super.print(' ');
	super.print(str);
    }

    // Output type long formatted in a given width
    public void print(long value)
    {
	output(String.valueOf(value));      // Pad to width and output
    }

    // Output type double fromatted in a given width
    public void print(double value)
    {
	output(String.valueOf(value));
    }

    // Output type long formatted in a given width plus a newline
    public void println(long value)
    {
	this.print(value);
	super.println();
    }

    // Output type double formatted in a given width plus a newline
    public void println(double value)
    {
	this.print(value);
	super.println();
    }

    public void setWidth(int width)
    {
	this.width = width > 0 ? width : 1;
    }
}
	
	
	

