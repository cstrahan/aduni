/****************************************************************************
 *  Title:  SameJava
 *  Contents:  An implementation of the board game "SameGame"
 *  JDK:  1.3
 *  Date:  1/18/00
 *  Notes:  Need to fix bug where peices are drawn circles, and you load a
 *          new image set, but menu box is not checked.
 *  Also:  This relies on the images from SameGnome being in
 *         /usr/share/pixmaps/same-gnome/planets.png.  Note that these image
 *         sets vary between 15 and 16 frames so i have just used 15.  Should
 *         really make this more robust.
 *  @author  Seth Plough
 ****************************************************************************/

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.MediaTracker;
import java.awt.Toolkit;
import java.awt.image.FilteredImageSource;
import java.awt.image.CropImageFilter;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.KeyEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.KeyStroke;
import javax.swing.BorderFactory;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import java.util.Vector;
import java.io.File;


/****************************************************************************
 *  The application begins here.
 ****************************************************************************/
public final class SameJava
{   
    public static void main(String[] args)
    {
        Board gameBoard = new Board();
    }
}


/*****************************************************************************
 *  Main game window, contains both the main JPanel (where balls are) and the
 *  score panel, as well as the menu.
 *****************************************************************************/
final class Board extends JFrame
{
    public final String VERSION = "1.0";
    public final Color BGColor = Color.black;             // The background color
    public final Color FGColor = new Color(255,255,204);  // The foreground color (score display, etc)
    public final Color COLOR1 = Color.gray;               // Default color for one of the 3 ball types
    public final Color COLOR2 = new Color(0,200,128);     // Default color for one of the 3 ball types
    public final Color COLOR3 = Color.blue;               // Default color for one of the 3 ball types
    public final int BALLSwidth = 15;                     // Number of columns of balls
    public final int BALLSheight = 10;                    // Number of rows of balls
    
    // Should check Screen Res. on start, resize if appropriate...
    public final int BOARDx = 750;  // 15*50
    public final int BOARDy = 545;  // 10*50 + 25(Menu) + some other stuff
    public final int BALLx = 50;                          // Width of each Ball in pixels
    public final int BALLy = 50;                          // Height of each Ball in pixels
    public final int PIECEdiameter = 40;                  // Width & height of the images for the Ball object
    public final int ANIMFrames = 15;                     // Number of frames in rotation for the animation
    public final boolean useBallBorder = true;            // Give each Ball a rectangular frame
    
    public boolean useImages = false;                     // Should Balls be images or just draw circles?
    public int currentFrame = 0;                          // The current frame in the animation
    public int ANIMRate = 100;                            // Number of milliseconds between animation frames
    public Image[] imageType1 = new Image[ANIMFrames];    // Array of frames for animation of Ball type 1
    public Image[] imageType2 = new Image[ANIMFrames];    // Array of frames for animation of Ball type 2
    public Image[] imageType3 = new Image[ANIMFrames];    // Array of frames for animation of Ball type 3
    public Animator animator;                             // New thread class to advance animation frames
    
    public Vector selectedBalls = new Vector();           // Vector of currently selected (highlighted) Balls
    public boolean removing = false;                      // Are we in the process of removing Balls?

    private Ball[][] balls;                               // Two dimensional array of the peices (balls)
    private gamePanel mainPanel;                          // JPanel to contain peices
    private scorePanel statsPanel;                        // JPanel for displaying score, etc.
    private JFrame game = this;                           // Reference to the uber-JFrame
    private JMenuBar menuBar;                             // The menu
    private JMenuItem menuItem1;                          // A menu choice:  Game menu : "New Game"
    private JMenuItem menuItemAbout;                      // A menu choice:  Game menu : "About"
    private JMenuItem menuItem2;                          // A menu choice:  File menu : "Load new image set"
    private JCheckBoxMenuItem mNoImages;                  // A menu choice:  File menu : "Use images"
    private MenuHandler menuHandler = new MenuHandler();  // A listener to process menu choices
    
    private int score = 0;                                // Current score
    private int type1 = 0;                                // Number of Balls of type 1 remaining
    private int type2 = 0;                                // Number of Balls of type 2 remaining
    private int type3 = 0;                                // Number of Balls of type 3 remaining
    

    /*****************************************************************************
     *  CONSTRUCTOR  
     *****************************************************************************/
    public Board()
    {
        // Need to instantiate this before calling loadImageSet (it will try to set this check box's state)
        mNoImages = new JCheckBoxMenuItem("Use images", true);  

        // use same-gnome's animations if its installed
        loadImageSet("/usr/share/pixmaps/same-gnome/planets.png");  
        
        balls = new Ball[BALLSwidth][BALLSheight];  // the 2D array of peices

        this.setSize(BOARDx,BOARDy+20);    // why +20?  Borders differed from machine to machine
        this.setTitle("SameJava");
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.getContentPane().setLayout(null);  // use absolute positioning
        this.getContentPane().setBackground( BGColor );


        // Set up the Menus
        menuBar = new JMenuBar();
        
        JMenu menu1 = new JMenu("Game");
        menuBar.add(menu1);

        menuItemAbout = new JMenuItem("About", KeyEvent.VK_A);
        menuItemAbout.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, ActionEvent.CTRL_MASK));
        menuItemAbout.addActionListener( menuHandler );
        menu1.add(menuItemAbout);

        menuItem1 = new JMenuItem("New Game", KeyEvent.VK_N);
        menuItem1.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
        menuItem1.addActionListener( menuHandler );
        menu1.add(menuItem1);

        JMenu menu2 = new JMenu("File");
        menuBar.add(menu2);

        menuItem2 = new JMenuItem("Load new image set", KeyEvent.VK_I);
        menuItem2.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I, ActionEvent.CTRL_MASK));
        menuItem2.addActionListener( menuHandler );
        menu2.add(menuItem2);

        mNoImages.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
        mNoImages.addActionListener( menuHandler );
        menu2.add(mNoImages);
        
        setJMenuBar(menuBar);        

        // Add the two panels and size them
        mainPanel = new gamePanel(true);
        statsPanel = new scorePanel();
        
        this.getContentPane().add(mainPanel);
        mainPanel.setBounds(0, 0, BOARDx, BOARDy - 45);
        this.getContentPane().add(statsPanel);
        statsPanel.setBounds(0, BOARDy - 45, BOARDx, 20);

        // Let's roll
        repaint();
        this.show();

        // Setup thread to handle animationn
        animator = new Animator();  
        animator.start();
    }


    /****************************************************************************
     *  Loads the image set (if possible), and breaks the frames into individual 
     *  images stored int 3 arrays.  Waits until the images are loaded
     *  (or it fails) to return.
     *  Note:  Animation frames must be 40x40 for now...
     *  @params name Pathname for imageset to load.  
     ****************************************************************************/
    public void loadImageSet(String name)
    {
        try
        {
            // First ensure that  this file exists and is readable
            File file = new File( name );
            if( file.canRead() )
            {
                System.out.println("Found image set to load:  " + name);        
        
                // Load up the large graphic that contains the animation frames
                Image imageSet = Toolkit.getDefaultToolkit().getImage(name);
        
                MediaTracker tracker = new MediaTracker(this);
                tracker.addImage(imageSet, 1);        
        
                // Load up the 3 animation frame sets
                for(int i=0; i<ANIMFrames; i++)
                {
                    imageType1[i] = createImage(new FilteredImageSource(imageSet.getSource(), 
                                                                        new CropImageFilter(i*40, 0, 40, 40)));
                    tracker.addImage(imageType1[i], i + 1);
                }

                for(int i=0; i<ANIMFrames; i++)
                {
                    imageType2[i] = createImage(new FilteredImageSource(imageSet.getSource(), 
                                                                        new CropImageFilter(i*40, 40, 40, 40)));
                    tracker.addImage(imageType2[i], i + ANIMFrames + 1);
                }

                for(int i=0; i<ANIMFrames; i++)
                {
                    imageType3[i] = createImage(new FilteredImageSource(imageSet.getSource(), 
                                                                        new CropImageFilter(i*40, 80, 40, 40)));
                    tracker.addImage(imageType3[i], i + ANIMFrames + ANIMFrames + 1);
                }

                tracker.waitForAll();  // wait here until images load
                useImages = true;  // use images only if they loaded correctly
                mNoImages.setState( true );
            }
            else  // we couldn't read this file
            {
                System.out.println("Couldn't find imageset:  " + name);
                useImages = false;  // revert to using circles instead of images
                mNoImages.setState( false );
            }
        }
        catch (InterruptedException e) 
        {
            System.out.println("Problem loading image set...");
            useImages = false;  // revert to using circles instead of images
            mNoImages.setState( false );
        }
        catch( SecurityException e )
        {
            System.out.println("Couldn't find imageset:  " + name + "\n" + e);
            useImages = false;  // revert to using circles instead of images
            mNoImages.setState( false );
        }

    }
    

    /*****************************************************************************
     *  @return Returns the player's current score.
     *****************************************************************************/
    public int getScore()
    {
        return score;
    }
    

    /*****************************************************************************
     *  @return Returns a random selected of one of the three ball colors.
     *****************************************************************************/
    public Color randColor()
    {
        int random = (int)(Math.random()*3);
        if(random == 0)
            return COLOR1;
        else if(random == 1)
            return COLOR2;
        else
            return COLOR3;
    }

    

    /*****************************************************************************
     *  Panel for display game stats.  Includes Score, number of balls of each
     *  type remaining, and number of balls selected (and which type they are)
     *****************************************************************************/
    public class scorePanel extends JPanel
    {
        /*****************************************************************************
         *  CONSTRUCTOR
         *****************************************************************************/
        public scorePanel()
        {
            this.setSize(BOARDx, 20);
            this.setBackground( BGColor );
            this.setVisible(true);
            if(useBallBorder)
            {
                this.setBorder( BorderFactory.createEtchedBorder( FGColor, Color.gray) );
            }
        }

        public void paintComponent(Graphics g)
        {
            super.paintComponent(g);
            g.setColor( FGColor );

            g.drawString("Score:  " + (getScore()), 15, 13);
            g.drawString("Type1:  " + type1, 200, 13);
            g.drawString("Type2:  " + type2, 300, 13);
            g.drawString("Type3:  " + type3, 400, 13);

            if( selectedBalls != null )
            {
                if( selectedBalls.size() > 0 )  // If anything is selected
                {
                    // Figure out which ball type is selected
                    Color c = ((Ball)selectedBalls.elementAt(0)).getColor();
                    int i;
                    if( c == COLOR1 ) i = 1;
                    else if( c == COLOR2 ) i = 2;
                    else if( c == COLOR3 ) i = 3;
                    else i = 0;
                    
                    g.drawString("# selected:  " + selectedBalls.size() + "   (Type" + i + ")", 500, 13);
                }
                else  // No balls selected
                {
                    g.drawString("# selected:  0", 500, 13);
                }
            }
        }
    }



    /*****************************************************************************
     *  The main panel which hold the Balls.
     *****************************************************************************/
    public final class gamePanel extends JPanel
    { 
        public gamePanel(boolean doubleBuffer)
        {   super(doubleBuffer);

            this.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            this.setLayout(null);

            /* Probably want to ensure at least 30-40 of each color...or not?  */               
            for(int i=0; i<BALLSwidth; i++)
            {   for(int j=0; j<BALLSheight; j++)
                {
                    balls[i][j] = new Ball( i, j );
                    balls[i][j].setColor(randColor());
                    this.add(balls[i][j]);
                    balls[i][j].setBounds(balls[i][j].getX(),
                                          balls[i][j].getY(),
                                          BALLx, BALLy);
                }
            }
        }

        public void paintComponent(Graphics g)
        {
            super.paintComponent(g);
        } //END: public void paintComponent


        /****************************************************************************
         *  Updates score to reflect the removal of the currently highlight balls.
         *  Players score the square of (the number of balls removed - 2).  Also
         *  updates the number of balls remaining.
         *****************************************************************************/
        public void updateScore()
        {
            if(selectedBalls.size() >= 2)
            {
                score += Math.pow((selectedBalls.size() - 2), 2);
                
                
                if(((Ball)selectedBalls.elementAt(0)).getColor() == COLOR1)
                    type1 -= selectedBalls.size();
                else if(((Ball)selectedBalls.elementAt(0)).getColor() == COLOR2)
                    type2 -= selectedBalls.size();
                else if(((Ball)selectedBalls.elementAt(0)).getColor() == COLOR3)
                    type3 -= selectedBalls.size();
            }
        }
        
        
        /****************************************************************************
         *  Handles process of removing highlighted balls and consolidating the remains.
         *****************************************************************************/        
        public void removeSelected()
        {
            // Set flag that shows the removal process is underway.
            //  (disables highlighting/dehighlighting)
            removing = true;

            updateScore();
            
            //  Test each ball to see if it's highlighted, and if it is, move
            //  each ball above it down one position.
            for(int i=0; i<BALLSwidth; i++)
            {   for(int j=0; j<BALLSheight; j++)
                {   if(balls[i][j].isSelected())
                    {
                        this.remove(balls[i][j]);  // remove the highlighted ball from the panel

                        // Loop through column, moving balls down one position
                        for(int k=j; k>0; k--)
                        {   balls[i][k] = balls[i][k-1];
                            balls[i][k].moveBall(0,1);
                            balls[i][k].setBounds(balls[i][k].getX(),
                                                  balls[i][k].getY(),
                                                  BALLx, BALLy);
                        }

                        // Add a nullball to the top of the row
                        balls[i][0] = new nullball(i, 0);
                        add(balls[i][0]);
                        balls[i][0].setBounds(balls[i][0].getX(),
                                              balls[i][0].getY(),
                                              BALLx, BALLy);
                    }
                }//END:  for
            }//END:  for

            // Consolidate balls if no more balls in
            //for(int i=0; i<BALLSwidth-1; i++)
            for(int i=BALLSwidth-1; i>=0; i--)
            {
                if(balls[i][BALLSheight-1].isNullBall())
                {
                    // find if there are any extant columns to our right...
                    int column = i;  // the rightmost row with real balls in it
                    for(int m=BALLSwidth-1; m>i; m--)
                    {
                        if( !(balls[m][BALLSheight-1].isNullBall()) )
                        {
                            column = m;
                            break;  // leave loop after find maximal column with non-null balls
                        }
                    }
                    
                    
                    if(column > i)
                    {
                        // remove balls from the null column
                        for(int count=0; count < BALLSheight; count++)
                        {
                            //getContentPane()
                            this.remove(balls[i][count]);
                        }

                        // shift balls over
                        for(int col=i; col<column; col++) 
                        {
                            for(int row=0; row<BALLSheight; row++)
                            {
                                balls[col][row] = balls[col+1][row];
                                balls[col][row].moveBall(-1,0);
                                balls[col][row].setBounds(balls[col][row].getX(),
                                                      balls[col][row].getY(),
                                                      BALLx, BALLy);
                            }
                        }

                        // add new null balls on rightside of the rightmost column moved
                        for(int count=0; count < BALLSheight; count++)
                        {
                            balls[column][count] = new nullball(column, count);
                            this.add(balls[column][count]);
                            balls[column][count].setBounds(balls[column][count].getX(),
                                                           balls[column][count].getY(),
                                                           BALLx, BALLy);    
                        }
                    }//END:  if(column>i)
                    
                }
            }//END:  for(int i=BALLSwidth-1; i>=0; i--)
            
            selectedBalls.removeAllElements();
            
            checkGameFinished();
            
            game.repaint();
            removing = false;  // return to normal highlighting/dehighlighting              
        }//END:  public void removeSelected()

        /****************************************************************************
         *  Sees if the game is over (check to see if there are legal moves left) and
         *  awards 1000 point if all balls are removed. 
         *****************************************************************************/
        public void checkGameFinished()
        {
            boolean done = true;

            if( balls[0][BALLSheight-1].isNullBall() )
            {
                score += 1000;
                endGame();
            }
            else
            {
                // search in checkerboard pattern
                for(int i=(BALLSheight-1); i>=0 && (done); i--)
                {
                    for(int j=(i%2); j<BALLSwidth && (done); j+=2)
                    {
                        done = !( balls[j][i].hasLikeNeighbor() );
                    }
                }
                
                if(done)  // game is over
                {
                    endGame();
                }
            }
        }

        public void endGame()
        {
            System.out.println("Game Finished!");
        }
    }//END: public class gamePanel
    
    



    /*******************************************************************************
     *  This class represents a piece on the board.  It knows its grid position, its
     *  color, whether or not it is selected (or a nullball), and how to draw itself.
     *  Balls handle their own MouseEvents.
     *******************************************************************************/
    class Ball extends JPanel
    {   
        private Point2D.Float coord;
        private Color ballColor = null;
        private boolean isSelected=false;
        private int xGrid;
        private int yGrid;
        private MouseHandler mhandle;
        private boolean isNullBall = false;
        private Image[] ballImage;
        private int ballFrameOffset = (int)(Math.random()*ANIMFrames);

        public Ball(int i, int j)
        {   
            xGrid = i;
            yGrid = j;
            coord = new Point2D.Float(i*BALLx, j*BALLy);
            this.setSize(BALLx, BALLy);
            if(useBallBorder)
                this.setBorder( BorderFactory.createEtchedBorder(new Color(255,255,204), Color.gray) );
            this.setBackground( BGColor );
            this.setDoubleBuffered(true);
            mhandle = new MouseHandler();
            this.addMouseListener(mhandle);
        }
        
        public void setColor(Color c)
        {
            ballColor = c;
            
            //if(useImages)
            {
                if(ballColor == COLOR1)
                {
                    ballImage = imageType1;
                    type1++;
                }
                else if(ballColor == COLOR2)
                {
                    ballImage = imageType2;
                    type2++;
                }
                else if(ballColor == COLOR3)
                {
                    ballImage = imageType3;
                    type3++;
                }
            }
        }
        
        
        public Color getColor()
        {   return ballColor;    
        }

        public void removeMouseHandler()
        {
            this.removeMouseListener(mhandle);
        }

        public int getX()
        {   return (int)coord.getX();
        }

        public int getY()
        {   return (int)coord.getY();
        }

        public boolean isSelected()
        {   return isSelected;
        }
        
        public void moveBall(int x, int y) //x and y displacement int grid units
        {
            xGrid += x;
            yGrid += y;
            coord.setLocation((coord.getX() + x*BALLx), (coord.getY() + y*BALLy));
            repaint();
        }
        

        public void paintComponent(Graphics g)
        {
            super.paintComponent(g);
            
            
            if(useImages && (ballImage != null)) // if we're using images
            {
                if( isSelected && (selectedBalls.size() > 1) )
                {
                    g.drawImage(ballImage[ (currentFrame+ballFrameOffset)%ANIMFrames ],
                                (BALLx - PIECEdiameter)/2,
                                (BALLy - PIECEdiameter)/2,
                                PIECEdiameter,
                                PIECEdiameter,
                                null);
                }
                else
                {
                    g.drawImage(ballImage[ballFrameOffset],
                                (BALLx - PIECEdiameter)/2,
                                (BALLy - PIECEdiameter)/2,
                                PIECEdiameter,
                                PIECEdiameter,
                                null);

                }
                
                    
            }
            
            else // if we aren't using images
            {
                Graphics2D g2 = (Graphics2D)g;
                Ellipse2D.Float circle = new Ellipse2D.Float((BALLx - PIECEdiameter)/2,
                                                             (BALLy - PIECEdiameter)/2,
                                                             PIECEdiameter,
                                                             PIECEdiameter);
                
                if( isSelected && (selectedBalls.size() > 1) )
                {
                    g2.setPaint( FGColor );
                    g2.fillRect(0, 0, BALLx, BALLy);
                }    

                g2.setPaint(ballColor);
                g2.fill(circle);
                
                g2.dispose(); // free up resources
            }
            
            
            g.dispose();
        }


        public boolean hasLikeNeighbor()
        {
            boolean right=false, left=false, up=false, down=false;
            
            if(xGrid < BALLSwidth-1)
            {   right = ( balls[xGrid + 1][yGrid].getColor() == ballColor );
            }
            if(xGrid > 0)
            {   left = ( balls[xGrid - 1][yGrid].getColor() == ballColor );
            }
            if(yGrid < BALLSheight-1)
            {   down = ( balls[xGrid][yGrid + 1].getColor() == ballColor );
            }
            if(yGrid > 0)
            {   up = ( balls[xGrid][yGrid - 1].getColor() == ballColor );
            }
            
            //System.out.println("xGrid:"+xGrid+"  yGrid:"+yGrid+"  r:"+right+"  l:"+left+"  u:"+up+"  d:"+down);
            return (right || left || up || down);
        }
        
            
        public void highlight(Color c)
        {
            if((c == ballColor) && (!isSelected) && (!removing))
            {
                selectedBalls.add(this);
                isSelected = true;
                if(xGrid < BALLSwidth-1)
                {   balls[xGrid + 1][yGrid].highlight(ballColor);
                }
                if(xGrid > 0)
                {   balls[xGrid - 1][yGrid].highlight(ballColor);
                }
                if(yGrid < BALLSheight-1)
                {   balls[xGrid][yGrid + 1].highlight(ballColor);
                }
                if(yGrid > 0)
                {   balls[xGrid][yGrid - 1].highlight(ballColor);
                }

                if( selectedBalls.size() > 1 )
                {
                    ballFrameOffset = (ballFrameOffset - currentFrame + ANIMFrames) % ANIMFrames;
                }
                
                repaint();
            }
        }



        public void dehighlight(Color c)
        {
            if((c == ballColor) && isSelected && (!removing))
            {
                isSelected = false;
                selectedBalls.remove(this);
                if(xGrid < BALLSwidth-1)
                {   balls[xGrid + 1][yGrid].dehighlight(ballColor);
                }
                if(xGrid > 0)
                {   balls[xGrid - 1][yGrid].dehighlight(ballColor);
                }
                if(yGrid < BALLSheight-1)
                {   balls[xGrid][yGrid + 1].dehighlight(ballColor);
                }
                if(yGrid > 0)
                {   balls[xGrid][yGrid - 1].dehighlight(ballColor);
                }
            }
            
            
            
            repaint();
        }
            
    

        private class MouseHandler implements MouseListener
        {
            public void mouseClicked(MouseEvent e)
            {
            }
            
            public void mousePressed(MouseEvent e)
            {
                if(selectedBalls.size() > 1)
                {
                    mainPanel.removeSelected();
                    // Highlight the ball where the mouse currently is
                    balls[xGrid][yGrid].highlight( balls[xGrid][yGrid].ballColor );
                }   
            }
            
            public void mouseReleased(MouseEvent e)
            {
            }
            
            public void mouseEntered(MouseEvent e)
            {   
                highlight(ballColor); // automatically the correct color
                repaint();
            }
            
            public void mouseExited(MouseEvent e)
            {   
                dehighlight(ballColor);
                repaint();
            }

        }//END:  private class MouseHandler


        public boolean isNullBall()
        {
            return isNullBall;
        }


        public void setNullBall(boolean b)
        {
            isNullBall = b;
            
        }        
        
    }//END:  class Ball


    /******************************************************************************
     *  A Ball object used to hold position in the "blank" spaces of the board.
     ******************************************************************************/
    public final class nullball extends Ball
    {
        public nullball(int i, int j)
        {
            super(i, j);
            super.setColor(Color.magenta);
            super.removeMouseHandler();
            super.setNullBall(true);
            this.addMouseListener(new MouseHandler());
        }

        public void highlight(Color c)
        {
        }

        public boolean hasLikeNeighbor()
        {   return false;
        }
        
        public void paintComponent(Graphics g)
        {
            super.paintComponent(g);
            g.setColor(BGColor);
            g.fillRect(0,0,BALLx,BALLy);
        }

        private class MouseHandler implements MouseListener
        {
            public void mouseClicked(MouseEvent e)
            {
            }

            public void mousePressed(MouseEvent e)
            {
            }
            
            public void mouseReleased(MouseEvent e)
            {
            }
            
            public void mouseEntered(MouseEvent e)
            {   
            }
            
            public void mouseExited(MouseEvent e)
            { 
            }

        }//END:  private class MouseHandler

    }//END:  public final class nullball extends Ball


    /******************************************************************************
     *  Takes care of parsing the user's menu choices.
     ******************************************************************************/
    public final class MenuHandler implements ActionListener
    {
        public void actionPerformed(ActionEvent e)
        {
            /******************************************************************************
             *  File menu : "Load new image set"
             *  Allows user to choose the image that contains animation frames to used.
             *  If an invalid image is chosen, the program will revert to drawing circular
             *  pieces.
             ******************************************************************************/
            if(e.getSource() == menuItem2)
            {
                JFileChooser chooser = new JFileChooser();
                chooser.setCurrentDirectory(new File("/usr/share/pixmaps/same-gnome"));

                int result = chooser.showOpenDialog(game);
                if(result == JFileChooser.APPROVE_OPTION)
                {
                    String filename = chooser.getSelectedFile().getPath();
                    if( (new File(filename)).exists() )
                    {
                        System.out.println("Loading image set.");
                        loadImageSet(filename);
                        game.repaint();
                        mainPanel.repaint();
                    }
                    else
                    {
                        System.out.println("Invalid image set.");
                        useImages = false;
                        mNoImages.setState( false );
                    }
                    
                }
            }
            else if(e.getSource() == menuItem1) // "new game"
            {              
                type1 = 0;
                type2 = 0;
                type3 = 0;
                score = 0;
                game.getContentPane().remove(mainPanel);
                mainPanel = new gamePanel(true);
                game.getContentPane().add(mainPanel);
                mainPanel.setBounds(0, 0, BOARDx, BOARDy - 45);
                selectedBalls.removeAllElements();
            }
            else if(e.getSource() == mNoImages) // "use images"
            {
                if(mNoImages.getState())
                {
                    if(imageType1[0] != null)  // see if images are already loaded
                    {
                        useImages = true;
                        mNoImages.setState( true );
                    }
                    else
                    {
                        loadImageSet("planets.png");
                    }
                }
                else
                {
                    useImages = false;
                    mNoImages.setState( false );
                }
                repaint();
            }
            else if(e.getSource() == menuItemAbout) // "About"
            {
                String message =
                    "Remove groupings of two or more balls.  If you get \n"
                    + "rid of x balls, you score the square of (x-2).  \n"
                    + "A bonus of 1000 is added for clearing all balls \n"
                    + "from the board.";
                JOptionPane.showMessageDialog(null , message, "SameJava (" + VERSION +") by Seth Plough", JOptionPane.INFORMATION_MESSAGE);
            }
        }//END:  actionPerformed
    }
    
    
    
    /*****************************************************************************
     *  This thread class updates the currentFrame variable (used in the painting
     *  of highlighted balls) and repaints.  The rate of animation is controlled
     *  by the ANIMRate variable.
     *****************************************************************************/
    public final class Animator extends Thread
    {
        public void run()
        {
            while(true)
            {
                currentFrame = (currentFrame + 1) % ANIMFrames;
                repaint();

                try
                {
                    sleep(ANIMRate);
                }
                catch(InterruptedException e)
                {
                    System.out.println("Animation thread had trouble sleeping.");
                }
                
            }
            
        }
    }//END:  Animator
    


    
}//END: public class SameJava



