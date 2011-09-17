import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.event.*;

class AboutDialog extends JDialog
{
  public AboutDialog(JFrame owner)
  {
    super(owner, "About SameJava", true);
    Container contentPane = getContentPane();

    // add HTML label to center

    contentPane.add(new JLabel
      ("<HTML><CENTER><H1><I>SameJava</I></H1><HR>"
       + "By Jeffrey M. Radcliffe</CENTER></HTML>"),
                    BorderLayout.CENTER);
    // Ok button to close the dialog
    JButton ok = new JButton("Ok");
    ok.addActionListener(new
      ActionListener()
      {
        public void actionPerformed(ActionEvent event)
        {
          setVisible(false);
        }
      });

    // Add ok button to southern border
    JPanel panel = new JPanel();
    panel.add(ok);
    contentPane.add(panel, BorderLayout.SOUTH);

    setSize(250, 150);
  }
}
