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
    JButton toggle = new JButton("Toggle");
    JButton green = new JButton("Green");
    JTextField text = new JTextField(10);


    // Button listener. Now an inner class so we
    //  can access mypanel data.
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

    MyPanel(){
	// make one instance of listener and add to all
	ButtonHandler b = new ButtonHandler();
	toggle.addActionListener(b);
	green.addActionListener(b);
	text.addActionListener(b);
	// add buttons (and text field)
	add(toggle);
	add(green);
	add(text);
    }

    public void paintComponent(Graphics g){
	super.paintComponent(g);

	//	    System.out.println("paintComponent");
	g.setColor(current);
	g.drawString(msg,50,100);
    }

}


public class WidgetTest{

    // OK let's have our main() create a frame.
    public static void main(String[] args){
	MyFrame myframe = new MyFrame();
	MyPanel mypanel = new MyPanel();
	// Random stuff we just have to do (the book explains, kind of)
	Container contentPane = myframe.getContentPane();
	// add panel
	contentPane.add(mypanel);
	myframe.show();
    }
}

/*


 */

