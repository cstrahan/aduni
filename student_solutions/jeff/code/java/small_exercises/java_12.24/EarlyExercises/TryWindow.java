import javax.swing.*;

public class TryWindow
{
    // The window object
    static JFrame aWindow = new JFrame("This is the Window Title");

    public static void main(String[] args)
    {
	aWindow.setBounds(50, 100, 400, 150);  // set position and size
	aWindow.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	aWindow.setVisible(true);
    }
}
	    
