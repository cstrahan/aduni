import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;

/**
 * Main class for game. Does window stuff and display
 */

class MyFrame extends JFrame{

    public MyFrame(){
	setTitle("WidgetTest");
	setSize(200,200);        // size in pixels
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

}



class MyPanel extends JPanel{

    // Color and string are now instance vars so we can change them
    Color current = Color.red;
    String msg = "Hello World";

    // button variables now instace vars.
    //  May as well init here.
    JTextField text = new JTextField(10);
    // Menu items created by menu.add(), still need to keep them for handler
    JMenuItem toggle;
    JMenuItem green;

    // We are using button Listener verbatim. Only difference
    //  is that we changed our buttons above to JMenuItems
    class ButtonHandler implements ActionListener{
	public void actionPerformed(ActionEvent e){
	    // check source, if green button, set color
	    if(e.getSource() == green){
		current = Color.green;
	    }
	    else if(e.getSource() == text){
		String s = text.getText();
		if(s != null) msg = s; // install as new msg
		repaint();
	    }
	    else{ // do toggle stuff
		if(current.equals(Color.red))
		    current = Color.blue;
		else
		    current = Color.red;
	    }
	    repaint();
	}
	
    }

    // Need myframe so we can add menubar 
    //  (could do in main and pass in menubar instead)
    MyPanel(JFrame myframe){
	// Need to add menu stuff and add to frame (not panel)
	JMenuBar mb = new JMenuBar();
	myframe.setJMenuBar(mb);
	// create menu and add to menuber
	JMenu cmenu = new JMenu("Colors");
	// Note: book is in error here, calls non-existant addMenu()
	mb.add(cmenu);
	// add() method on menu creates and adds menu items
	toggle = cmenu.add("Toggle");
	// add a horiz line to look cool
	cmenu.addSeparator();
	green = cmenu.add("Green");
	// make one instance of listener and add to all
	//  works exactly the same as for buttons
	ButtonHandler b = new ButtonHandler();
	toggle.addActionListener(b);
	green.addActionListener(b);
	text.addActionListener(b);

	// add text field
	add(text);
    }

    public void paintComponent(Graphics g){
	super.paintComponent(g);

	//	    System.out.println("paintComponent");
	g.setColor(current);
	g.drawString(msg,50,100);
    }

}


public class MenuTest{

    // OK let's have our main() create a frame.
    public static void main(String[] args){
	MyFrame myframe = new MyFrame();
	MyPanel mypanel = new MyPanel(myframe);
	// Random stuff we just have to do (the book explains, kind of)
	Container contentPane = myframe.getContentPane();
	// add panel
	contentPane.add(mypanel);
	myframe.show();
    }
}

/*


 */

