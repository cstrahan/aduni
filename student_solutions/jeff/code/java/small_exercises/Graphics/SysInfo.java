import java.awt.*;

public class SysInfo
{
    public static void main(String[] args)
    {
	Toolkit theKit = Toolkit.getDefaultToolkit();

	System.out.println("\nScreen Resolution: "
			   + theKit.getScreenResolution() + " dots per inch");

	Dimension screenDim = theKit.getScreenSize();
	System.out.println("Screen Size: "
			    + screenDim.width + " byte "
			    + screenDim.height +  " pixels");

	GraphicsEnvironment e = GraphicsEnvironment.getLocalGraphicsEnvironment();
	String[] fontnames = e.getAvailableFontFamilyNames();
	System.out.println("\nFonts available on this platform: ");
	for(int i = 0; i < fontnames.length; i++)
	    System.out.println(fontnames[i]);

	return;
    }
}
