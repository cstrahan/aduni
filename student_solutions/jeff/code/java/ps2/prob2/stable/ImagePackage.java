/**
 * SameJava -- a same-gnome/same-game clone
 * This file is ImagePackage.java, which contains the default image information.
 *
 * @author Jeffrey M. Radcliffe
 * @version $Id: ImagePackage.java,v 1.2 2001/01/17 15:34:12 jeff Exp $
 *
*/
import java.awt.*;

abstract public class ImagePackage
{
  /**
     Returns the list of images
     @return a String[6] containing file locations of images.
  */
  abstract public String[] getImages();
  /**
     Returns labels which the ScorePanel uses.
     @return a String[3] of short labels.
  */
  abstract public String[] getShortDescription();
  /**
     Provides the default background color (default is white)
     @return a color
  */
  public Color getColor() {
    return new Color(255 ,255 ,255);
  }

  /**
     Provides the default background color for the ScorePanel
  */
  public Color getScorePanelColor() {
    return new Color(255, 255, 255);
  }
}

/**
   Tux the penguin, the GNU, and the BSD Daemon.
*/
class OSImagePackage extends ImagePackage
{
  public String[] getImages() {
    return imageSet;
  }

  public String[] getShortDescription() {
    return shortDescription;
  }

  private String[] imageSet = 
  { "images/tux.gif",
    "images/gnu.gif",
    "images/bsd2.gif",
    "images/tux_highlight.gif",
    "images/gnu_highlight.gif",
    "images/bsd2_highlight.gif"};

  private String[] shortDescription =
  {
    "Tux", "Gnu", "Daemon"
  };
}

/**
   Some gemmy-things (courtesy V. Cartledge)
*/
    class GemImagePackage extends  ImagePackage
    {
      public String[] getImages() {
        return imageSet;
      }

      public String[] getShortDescription() {
        return shortDescription;
      }

      private static String[] imageSet = 
        { "images/tile1.jpg",
          "images/tile2.jpg",
          "images/tile3.jpg",
          "images/tile4.jpg",
          "images/tile5.jpg",
          "images/tile6.jpg"};
      private String[] shortDescription =
      {
        "Green", "Red", "Blue"
      };
    }


/**
   Trippy flowers (courtesy V. Cartledge)
*/
class FlowerImagePackage extends  ImagePackage
{
  public String[] getImages() {
    return imageSet;
  }

  public String[] getShortDescription() {
    return shortDescription;
  }

  private static String[] imageSet = 
    { "images/rose1.jpg",
      "images/violet1.jpg",
      "images/daisy1.jpg",
      "images/rose2.jpg",
      "images/violet2.jpg",
      "images/daisy2.jpg"};
  private String[] shortDescription =
  {
    "Rose", "Violet", "Daisy"
  };

}

/**
   Can you be the one? Images from the NewLine film
   "The Matrix" (C) Glowy eyes mine.
*/
class MatrixImagePackage extends ImagePackage{

  public String[] getImages() {
    return imageSet;
  }

  public String[] getShortDescription() {
    return shortDescription;
  }

  /** Overrides super */
  public Color getColor() {
    return new Color(0 ,0 ,0);
  }

  /** Overrides super */
  public Color getScorePanelColor() {
    return new Color(207 ,207 ,207);
  }

  private static String[] imageSet = 
    { "images/neo.jpg",
      "images/trinity.jpg",
      "images/morpheus.jpg",
      "images/neo_highlight.jpg",
      "images/trinity_highlight.jpg",
      "images/morpheus_highlight.jpg"};

  private String[] shortDescription =
  {
    "Neo", "Trinity", "Morpheus"
  };
}

/**
   Simple yet elegant pieces (courtesy V. Cartledge)
*/
class ElegantImagePackage extends ImagePackage{

  public String[] getImages() {
    return imageSet;
  }

  public String[] getShortDescription() {
    return shortDescription;
  }

  /** Overrides super */
  public Color getColor() {
    return new Color(0 ,0 ,0);
  }

  /** Overrides super */
  public Color getScorePanelColor() {
    return new Color(207 ,207 ,207);
  }

  private static String[] imageSet = 
    { "images/oval1.jpg",
      "images/oval2.jpg",
      "images/oval3.jpg",
      "images/oval4.jpg",
      "images/oval5.jpg",
      "images/oval6.jpg"};

  private String[] shortDescription =
  {
    "I", "II", "III"
  };
}
