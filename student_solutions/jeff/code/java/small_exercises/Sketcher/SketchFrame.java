// Frame for the Sketcher application
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class SketchFrame extends JFrame implements Constants
{
  // Constructor
  public SketchFrame(String title)
  {
    setTitle(title);                          // set window title
    setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

    setJMenuBar(menuBar);

    JMenu fileMenu = new JMenu("File");
    JMenu elementMenu = new JMenu("Elements");
    fileMenu.setMnemonic('F');                        // Create shortcut
    elementMenu.setMnemonic('E');                     // Create shortcut

    // Construct the file pull down menu
    newItem = fileMenu.add("New");
    openItem = fileMenu.add("Open");
    closeItem = fileMenu.add("Close");
    fileMenu.addSeparator();
    saveItem = fileMenu.add("Save");
    saveAsItem = fileMenu.add("Save As...");
    fileMenu.addSeparator();
    printItem = fileMenu.add("Print");

    // Accelerators for the file Menu
    newItem.setAccelerator(KeyStroke.getKeyStroke('N', Event.CTRL_MASK));
    openItem.setAccelerator(KeyStroke.getKeyStroke('O' ,Event.CTRL_MASK));
    saveItem.setAccelerator(KeyStroke.getKeyStroke('S' , Event.CTRL_MASK));
    printItem.setAccelerator(KeyStroke.getKeyStroke('P', Event.CTRL_MASK));

    // Construct the Elements pull down menu
    elementMenu.add(lineItem = new JRadioButtonMenuItem
      ("Line", elementType == LINE));
    elementMenu.add(rectangleItem = new JRadioButtonMenuItem
      ("Rectangle", elementType == RECTANGLE));
    elementMenu.add(circleItem = new JRadioButtonMenuItem
      ("Circle", elementType == CIRCLE));
    elementMenu.add(curveItem = new JRadioButtonMenuItem
      ("Curve", elementType == CURVE ));
    elementMenu.add(textItem = new JRadioButtonMenuItem("Text", false));
    ButtonGroup types = new ButtonGroup();
    types.add(lineItem);
    types.add(rectangleItem);
    types.add(circleItem);
    types.add(curveItem);
    types.add(textItem);

    // Accelarators for the shapes
    lineItem.setAccelerator(KeyStroke.getKeyStroke('L', Event.CTRL_MASK));
    rectangleItem.setAccelerator(KeyStroke.getKeyStroke('E', Event.CTRL_MASK));
    circleItem.setAccelerator(KeyStroke.getKeyStroke('I', Event.CTRL_MASK));
    curveItem.setAccelerator(KeyStroke.getKeyStroke('V', Event.CTRL_MASK));

    // Add type menu item listeners
    lineItem.addActionListener(new TypeListener(LINE));
    rectangleItem.addActionListener(new TypeListener(RECTANGLE));
    circleItem.addActionListener(new TypeListener(CIRCLE));
    curveItem.addActionListener(new TypeListener(CURVE));
    	
    elementMenu.addSeparator();
	
    JMenu colorMenu =  new JMenu("Color");
    elementMenu.add(colorMenu);
    colorMenu.add(redItem = new JCheckBoxMenuItem
      ("Red", elementColor.equals(Color.red)));
    colorMenu.add(yellowItem = new JCheckBoxMenuItem
      ("Yellow",elementColor.equals(Color.yellow)));
    colorMenu.add(greenItem = new JCheckBoxMenuItem
      ("Green",elementColor.equals(Color.green)));
    colorMenu.add(blueItem = new JCheckBoxMenuItem
      ("Blue",elementColor.equals(Color.blue)));

    // Add color menu item listeners
    redItem.addActionListener(new ColorListener(Color.red));
    yellowItem.addActionListener(new ColorListener(Color.yellow));
    greenItem.addActionListener(new ColorListener(Color.green));
    blueItem.addActionListener(new ColorListener(Color.blue));
    
    // Accelerators for color menu
    redItem.setAccelerator(KeyStroke.getKeyStroke('R' , Event.CTRL_MASK));
    yellowItem.setAccelerator(KeyStroke.getKeyStroke('Y', Event.CTRL_MASK));
    greenItem.setAccelerator(KeyStroke.getKeyStroke('G' , Event.CTRL_MASK));
    blueItem.setAccelerator(KeyStroke.getKeyStroke('B' , Event.CTRL_MASK));
    
    ButtonGroup colors = new ButtonGroup();
    colors.add(redItem);
    colors.add(yellowItem);
    colors.add(greenItem);
    colors.add(blueItem);

    menuBar.add(fileMenu);
    menuBar.add(elementMenu);
  }
  // Handles color menu items
  class ColorListener implements ActionListener
  {
    public ColorListener(Color color)
    {
      this.color = color;
    }
    
    public void actionPerformed(ActionEvent e)
    {
      elementColor = color;
    }

    private Color color;
  }
  
  // Handles element type menu items
  class TypeListener implements ActionListener
  {
    // Constructor
    TypeListener(int type)
    {
      this.type = type;
    }

    // Sets the element type
    public void actionPerformed(ActionEvent e)
    {
      elementType = type;
    }

    private int type; // Store the type for the menu
  }
	   
  private JMenuBar menuBar = new JMenuBar();

    // File menu items
  private JMenuItem newItem, openItem, closeItem, fileItem,
    saveItem, saveAsItem, printItem;
  private JRadioButtonMenuItem lineItem, rectangleItem, circleItem,
    curveItem, textItem;
  private JCheckBoxMenuItem redItem, yellowItem, greenItem, blueItem;
  private Color elementColor = DEFAULT_ELEMENT_COLOR;
  private int elementType = DEFAULT_ELEMENT_TYPE;
}
