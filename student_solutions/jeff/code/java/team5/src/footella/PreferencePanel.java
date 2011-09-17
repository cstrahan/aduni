import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;
import java.util.*;

public class PreferencePanel extends JPanel implements Pokable
{
  PreferencePanel(ControlHandler control, Preferences p) {
    this.control = control;
    this.prefs = p;
    this.prefsRec = prefs.getPrefsRecord();

    Box fooBox = Box.createVerticalBox();

    minSpeed = new JTextField
      (new String(prefsRec.getMinSpeed() + ""), 10);
    JPanel aPanel = new JPanel();
    aPanel.add(new JLabel("Connection Speed:"));
    aPanel.add(minSpeed);
    fooBox.add(aPanel);

    downloadPath = new JTextField
      (new String(prefsRec.getDownloadPath()), 20);
    aPanel = new JPanel();
    aPanel.add(new JLabel("Download Path:"));
    aPanel.add(downloadPath);
    fooBox.add(aPanel);

    searchPath = new JTextField
      (new String(prefsRec.getSearchPath()), 20);
    aPanel = new JPanel();
    aPanel.add(new JLabel("Seach Path:"));
    aPanel.add(searchPath);
    fooBox.add(aPanel);

    ttlPref = new JTextField
      (new String(prefsRec.getTTL() + ""), 10);
    aPanel = new JPanel();
    aPanel.add(new JLabel("Time To Live:"));
    aPanel.add(ttlPref);
    fooBox.add(aPanel);

    portNum = new JTextField
      (new String(prefsRec.getPortNum() + ""), 10);
    aPanel = new JPanel();
    aPanel.add(new JLabel("Connection Port:"));
    aPanel.add(portNum);
    fooBox.add(aPanel);


    JButton saveButton = new JButton("Save Preferences");
    saveButton.addActionListener(new
      AbstractAction() {
        public void actionPerformed(ActionEvent e) {
          savePrefs();
        }
      });

    fooBox.add(saveButton);
    aPanel = new JPanel();
    aPanel.setBorder(BorderFactory.createTitledBorder("Preferences"));
    aPanel.add(fooBox);
    add(aPanel);

  }

  public void savePrefs() {
    int speed = Integer.parseInt(minSpeed.getText().trim());
    int ttl = Integer.parseInt(ttlPref.getText().trim());
    String dl = downloadPath.getText();
    String sp = searchPath.getText();
    Vector v = new Vector();
    v.add(new String("gnutellahosts.com"));
    int port =  Integer.parseInt(portNum.getText().trim());
    //    prefs.savePrefs(speed, ttl, prefs.prefrec.hosts, dl, sp, port);
    prefs.savePrefs(speed, ttl, prefs.prefrec.hosts, dl, sp, port);
    System.out.println("Save successful.");
  }

  public void poke() {}

  // fields
  private JTextField minSpeed;
  private JTextField downloadPath;
  private JTextField searchPath;
  private JTextField ttlPref;
  private JTextField portNum;
  private ControlHandler control;
  private Preferences prefs;
  private PrefsRecord prefsRec;
}

