import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

public class PreferencePanel extends JPanel implements Pokable
{
  PreferencePanel(ControlHandler control, Preferences pref) {
    this.control = control;
    this.prefs = prefs;
    this.prefsRec = pref.getPrefsRecord();

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
    prefs.savePrefs(Integer.parseInt(minSpeed.getText().trim()),
                    Integer.parseInt(minSpeed.getText().trim()),
                     prefsRec.getHosts(),
                     downloadPath.getText().trim(),
                     searchPath.getText().trim(),
                    Integer.parseInt(minSpeed.getText().trim()));
        }
      });

    fooBox.add(saveButton);
    aPanel = new JPanel();
    aPanel.setBorder(BorderFactory.createTitledBorder("Preferences"));
    aPanel.add(fooBox);
    add(aPanel);

  }

  public void savePrefs() {
    prefs.savePrefs(Integer.parseInt(minSpeed.getText().trim()),
                    Integer.parseInt(ttlPref.getText().trim()),
                     prefsRec.getHosts(),
                     downloadPath.getText().trim(),
                     searchPath.getText().trim(),
                    Integer.parseInt(minSpeed.getText().trim()));
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

