package sunw.demo.misc;


import java.awt.*;
import java.awt.event.*;


public class DemoNestedClasses extends Frame
{

  Button startButton = new Button("Start Juggling");
  Button stopButton = new Button("Stop Juggling");
  Juggler juggler = new Juggler();

  DemoNestedClasses()
  {
    super("Demo Nested Class");
    setLayout(new FlowLayout());
    add(startButton);
    add(stopButton);
    add(juggler);

    startButton.addActionListener(new StartButtonListener());
    stopButton.addActionListener(new StopButtonListener());
  }

  class StartButtonListener implements ActionListener
  {
    public void actionPerformed(ActionEvent e) 
    {
      juggler.start(e);
    }
  } 

  class StopButtonListener implements ActionListener
  {
    public void actionPerformed(ActionEvent e) 
    {
      juggler.stop(e);
    }
  } 

  public static void main(String args[])
  {
    Frame f = new DemoNestedClasses();
    f.pack();
    f.show();
  }
}

