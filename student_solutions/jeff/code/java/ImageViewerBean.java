import java.awt.*;
import java.io.*;
import javax.swing.*;

public class ImageViewerBean extends JPanel
implements Serializable
{
  public void setFileName(String f) {
    fileName = f;
    image = Toolkit.getDefaultToolkit().getImage(fileName);
    MediaTracker tracker = new MediaTracker(this);
    tracker.addImage(image, 0);
    try { tracker.waitForID(0); } 
    catch(InterruptedException e) {}
    repaint();
  }
  public String getFileName(){
    return fileName;
  }

  public void paint(Graphics g) {
    if (image == null) {
      g.drawRect(0, 0, getWidth() - 1, getHeight() - 1);
    } else
      g.drawImage(image, 0, 0, this);
  }
  public Dimension getPreferredSize() {
    if(image == null)
      return new Dimension(MINSIZE, MINSIZE);
    return new Dimension(image.getWidth(null),
                         image.getHeight(null));
  }

  private static final int MINSIZE = 50;
  private Image image = null;
  private String fileName = "";
}
    
   
      
  


