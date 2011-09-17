// Applet to generate lottery entries
import javax.swing.*;
import javax.swing.border.*;
import java.util.Random;
import java.awt.*;
import java.awt.event.*;

public class Lottery extends JApplet
{
  // Generate numberCount random selections from the values array
  static int[] getNumbers()
  {
    int[] numbers = new int[numberCount];
    int candidate = 0;
    for(int i = 0; i < numberCount; i++)
    {
    search:
      for(;;)
      {
	candidate = values[choice.nextInt(values.length)];
	for(int j = 0 ; j < i ; j++)
	  if(candidate == numbers[j])
	    continue search;

	numbers[i] = candidate;
	break;
      }
    }
    return numbers;
  }
  
			
  // Initialize the applet
  public void init()
  {
    // Set up the selection buttons
    Container content = getContentPane();
    content.setLayout(new GridLayout(0,1));  // Set the layout for the applet


    // Add the pane containing control buttons
    JPanel controlPane = new JPanel(new FlowLayout(FlowLayout.CENTER, 5, 10));  

    // Add the two control buttons
    JButton button;                                      // A button variable
    Dimension buttonSize = new Dimension(100,20);        // Button size

    controlPane.add(button = new JButton("Lucky Numbers!"));
    button.setBorder(BorderFactory.createRaisedBevelBorder());
    button.addActionListener(new HandleControlButton(PICK_LUCKY_NUMBERS));
    button.setPreferredSize(buttonSize);

    controlPane.add(button = new JButton("Color"));
    button.setBorder(BorderFactory.createRaisedBevelBorder());
    button.addActionListener(new HandleControlButton(COLOR));
    button.setPreferredSize(buttonSize);

    content.add(controlPane);
    

    // Set up the panel to hold the lucky number buttons
    JPanel buttonPane = new JPanel();  // Add the pane containing numbers

    // Let's have a fancy panel border
    buttonPane.setBorder(BorderFactory.createTitledBorder(
                         BorderFactory.createEtchedBorder(Color.cyan,
                                                          Color.blue),
                                                          "Every One a Winner!"));

    int[] choices = getNumbers();            // Get initial set of numbers
    for(int i = 0; i<numberCount; i++)
    {
     luckyNumbers[i] = new Selection(choices[i]);
     buttonPane.add(luckyNumbers[i]);
    }
    content.add(buttonPane);

    // Set up the control buttons...
  }

 
  // Custom button showing lottery selection
  
  // Each button listens for its own events
  class Selection extends JButton
                  implements ActionListener
  {
    // Constructor
    public Selection(int value)
    {
      super(Integer.toString(value));    // Call base constructor and set the label
      this.value = value;                // Save the value
      setBackground(startColor);
      setBorder(BorderFactory.createRaisedBevelBorder());    // Add button border
      setPreferredSize(new Dimension(80,20));
      addActionListener(this);           // Button listens for itself
    }

    // Handle selection button event
    public void actionPerformed(ActionEvent e)
    {
      // Change this selection to a new selection
      int candidate = 0;
      for(;;)                                // Loop to find a different selection
      {
        candidate = values[choice.nextInt(values.length)];
        if(isCurrentSelection(candidate))    // If it is not different
          continue;                          // find another
        setValue(candidate);                 // We have one so set the button value
        return;
      }
    }

    // Set the value for the selection
    public void setValue(int value)
    {
      setText(Integer.toString(value));    // Set value as the button label
      this.value = value;                   // Save the value
    }

    // Check the value for the selection
    boolean hasValue(int possible)
    {
      return value==possible;               // Return true if equals current value 
    }

    // Check the current choices
    boolean isCurrentSelection(int possible)
    {
      for(int i = 0; i < numberCount; i++)         // For each button
        if(luckyNumbers[i].hasValue(possible))     // check against possible
          return true;                             // Return true for any =
      return false;                                // Otherwise return false 
    }

    private int value;                       // Value for the selection button
  }



  // Class defining a handler for a control button
  class HandleControlButton implements ActionListener
  {
    private int buttonID;   

    // Constructor
    public HandleControlButton(int buttonID)
    {
      this.buttonID = buttonID;                   // Store the button ID
    }

    // Handle button click
    public void actionPerformed(ActionEvent e)
    {
      switch(buttonID)
      {
        case PICK_LUCKY_NUMBERS:
          int[] numbers = getNumbers();            // Get maxCount random numbers
          for(int i = 0; i < numberCount; i++)
            luckyNumbers[i].setValue(numbers[i]);  // Set the button values
          break;
        case COLOR:
          Color color = new Color(
          flipColor.getRGB()^luckyNumbers[0].getBackground().getRGB());
          for(int i = 0; i < numberCount; i++)
            luckyNumbers[i].setBackground(color);  // Set the button colors
          break;
      }
    }
  }


  final static int numberCount = 6;                   // Number of lucky numbers
  final static int minValue = 1;                      // Minimum in range
  final static int maxValue = 49;                     // Maximum in range
  static int[] values = new int[maxValue-minValue+1]; // Array of possible values

  static                                              // Initialize array
  {
    for(int i = 0 ; i<values.length ; i++)
      values[i] = i + minValue;
  }

  // An array of custom buttons for the selected numbers
  private Selection[] luckyNumbers = new Selection[numberCount]; 

  final public static int PICK_LUCKY_NUMBERS = 1;                 // Select button ID
  final public static int COLOR = 2;                              // Color button ID

  Color flipColor = new Color(
                      Color.yellow.getRGB()^Color.red.getRGB());  // swap colors

  Color startColor = new Color(Color.yellow.getRGB());            // start color


  private static Random choice = new Random();        // Random number generator
}






