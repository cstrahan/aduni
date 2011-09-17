package org.aduni.dg;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.event.*;

/**
 * Main class for game. Does window stuff and display
 */

class MyFrame extends JFrame{

    public MyFrame(){
	setTitle("MyFrame");
	setSize(400,400);        // size in pixels
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

}



class MyPanel extends JPanel{

    public GridBagConstraints makeConstraints(int x, int y, int w, int h){
	GridBagConstraints c = new GridBagConstraints();
	c.weightx = 100;
	c.weighty = 100;
	c.gridx = x;
	c.gridy = y;
	c.gridwidth = w;
	c.gridheight = h;
	c.fill = GridBagConstraints.BOTH;
	return(c);
    }

    public void initCon(GridBagConstraints[] con){
	con[0] = makeConstraints(0,0,1,1);
	con[1] = makeConstraints(2,0,2,1);
	con[2] = makeConstraints(0,1,2,1);
	con[3] = makeConstraints(2,1,1,1);
	con[4] = makeConstraints(0,2,4,1);
	con[5] = makeConstraints(0,3,1,2);
	con[6] = makeConstraints(1,3,3,2);
	con[7] = makeConstraints(0,5,1,1);
	con[8] = makeConstraints(1,5,1,1);
	con[9] = makeConstraints(2,5,1,1);
	con[10] = makeConstraints(3,5,1,1);
    }

    public Box doBox(){
	Box[] boxes = new Box[3];
	boxes[0] = Box.createHorizontalBox();
	boxes[1] = Box.createHorizontalBox();
	boxes[2] = Box.createHorizontalBox();
	Box vbox = Box.createVerticalBox();
	// add buttons and glue
	for(int i=0;i<11;i++){
	    boxes[i%3].add(buttons[i]);
	    if(i%2 == 1)
		boxes[i%3].add(Box.createHorizontalGlue());
	}
	// stack hboxes in vbox with a 50 pixel strut and glue
	vbox.add(boxes[0]);
	vbox.add(Box.createVerticalStrut(50));
	vbox.add(boxes[1]);
	vbox.add(Box.createVerticalGlue());
	vbox.add(boxes[2]);
	return(vbox);
    }

    Color colors[] ={ Color.red, Color.blue, Color.yellow, Color.green, 
		      Color.pink, Color.lightGray, Color.white, Color.gray, 
		      Color.orange, Color.cyan,Color.magenta};
    String[] names = {"Sleepy","Dopey","Grumpy","Sneezy","S.White",
		      "Donner","Blitzen","Rudolph",
		      "Mike","Rusty","Dimitri"};
    String[] pos = {"Center","North","East","South","West",
		    "Center","North","East","South","West",
		    "Center","North","East","South","West"};
    JButton[] buttons = new JButton[11];
    BorderLayout bd = new BorderLayout();
    GridLayout grid = new GridLayout(3,4);
    GridBagLayout gb = new GridBagLayout();
    JPanel[] panels = new JPanel[5];
    GridBagConstraints con[] = new GridBagConstraints[11];

    MyPanel(){
	//	setLayout(bd);

	//	setLayout(grid);

	//	setLayout(gb);
	//	initCon(con);
	//	setLayout(null);

	//	for(int i=0;i<5;i++){
	//	    panels[i] = new JPanel();
	//	    panels[i].setLayout(new GridLayout(2,2));
	//	    add(panels[i],pos[i]);
	//	}
	for(int i=0;i<11;i++){
	    buttons[i] = new JButton(names[i]);
	    buttons[i].setBackground(colors[i]);
	    add(buttons[i]);
	    buttons[i].setBounds(i*40,i*40,100,30);
	    //	    panels[i%5].add(buttons[i]);
	    //	    add(buttons[i],con[i]);
	    //add(buttons[i],pos[i]);
	}
	//	add(doBox(),"Center");
    }

    public void paintComponent(Graphics g){
	super.paintComponent(g);

	//	    System.out.println("paintComponent");
    }

}


public class LayoutTest{

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

