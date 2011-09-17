import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;

/**
 *  Problem 2, Problem set 2, Java/OOP.
 *  Design a Same-style game on the computer using GUI, Swing, and
 *  other goodies/classes.
 *  @author Tom Hickerson, January 2001.
 */
public class SameJava
{
    
    public static void main(String[] args)
    {
	SFrame sBoard = new SFrame();
	SPanel mainPanel = new SPanel(sBoard);
	Container contentpane = sBoard.getContentPane();
	contentpane.add(mainPanel);
	sBoard.show();
    }
}

/**
 *  Class SFrame: creates a frame.
 *  @param B_WIN_X A constant, the x-value of the game window.
 *  @param B_WIN_Y A constant, the y-value of the game window.
 */
class SFrame extends JFrame
{
    public final int B_WIN_X = 600;
    public final int B_WIN_Y = 425; //ten balls and menu bar
     
    public SFrame()
    {
       	setTitle("Tom's SameJava");
	setSize(B_WIN_X,B_WIN_Y);
	
	setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }
}

/**
 *  Class SPanel: creates a panel which carries the game board.
 *  @param BCKGND A Constant Color, the background
 *  @param COLOR1 A Constant Color, the first game ball color
 *  @param COLOR2 A Constant Color, the second game ball color
 *  @param COLOR3 A Constant Color, the third game ball color
 *  @param c1 A Constant Color, the first highlight color
 *  @param c2 A Constant Color, the second highlight color
 *  @param c3 A Constant Color, the third highlight color
 *  @param B_WIDTH The width of the game int game balls
 *  @param B_HEIGHT The height of the game int game balls
 *  @param BALL_X The width of the game piece
 *  @param BALL_Y The height of the game piece
 *  @param balls The 2-dimensional array of game balls
 *  @param score The game score
 *  @param scount The count of how many pieces are highlighted 
 *  at one time
 */
class SPanel extends JPanel
{
    public final Color BCKGND = Color.black;
    public final Color COLOR1 = Color.red;
    public final Color COLOR2 = Color.cyan;
    public final Color COLOR3 = Color.blue;
    
    public Color c3 = new Color(40,10,128);
    public Color c1 = new Color(128,40,10);
    public Color c2 = new Color(0,128,40);
				
    public final int B_WIDTH = 15;
    public final int B_HEIGHT = 10;
    public final int BALL_X = 40;
    public final int BALL_Y = 40;

    Ball[][] balls;
    public int score = 0;
    public int scount;

    JMenuItem newgame;
    JMenuItem quitgame;

    public SPanel(JFrame sframe)
    {
	balls = new Ball[B_WIDTH][B_HEIGHT];
	setBackground(Color.black);

	JMenuBar mb = new JMenuBar();
	sframe.setJMenuBar(mb);
	JMenu smenu = new JMenu("File");
	mb.add(smenu);
	smenu.setMnemonic('F');
	
	newgame = smenu.add("New Game");
	newgame.setMnemonic('N');
	smenu.addSeparator();
	quitgame = smenu.add("Quit Game");
	quitgame.setMnemonic('Q');
	ButtonHandler b = new ButtonHandler();
	quitgame.addActionListener(b);
	newgame.addActionListener(b);
	resetBoard();

	addMouseListener(new MouseHandler());
	addMouseMotionListener(new MouseMotionHandler());
    }

    /**
     *  resetBoard -- resets the entire board, and repaints.
     */
    public void resetBoard()
    {
	score = 0;
	for (int i = 0; i < B_WIDTH; i++)
	    {
		for (int j = 0; j < B_HEIGHT; j++)
		    {
			balls[i][j] = new Ball();
			balls[i][j].setColor(randColor());
			balls[i][j].setcColor(balls[i][j].getColor());
		    }
	    }
	repaint();
    }

    /**
     *  ButtonHandler -- governs the menu bar
     */
    public class ButtonHandler implements ActionListener
    {
	public void actionPerformed(ActionEvent ev)
	{
	    if (ev.getSource() == quitgame) System.exit(0);
	    if (ev.getSource() == newgame) resetBoard();
	}
    }
	
    /**
     *  randColor -- provides a random color for the game pieces.
     */
    public Color randColor()
    {
	int rand = (int) (Math.random() * 3);
	if (rand == 0)
	    return COLOR1;
	else if (rand == 2)
	    return COLOR2;
	else 
	    return COLOR3;
    }

    /**
     * the paintComponent -- paints the board every repaint() call.
     */
    public void paintComponent(Graphics g)
    {
      	super.paintComponent(g);
	Graphics2D g2 = (Graphics2D)g;
        for (int i = 0; i < B_WIDTH; i++)
	    {
		for (int j = 0; j < B_HEIGHT; j++)
		    {
			Rectangle2D rect = new Rectangle2D.Double((i * 40) + 1,(j * 40) + 1,38,38);
			g2.setPaint(balls[i][j].getColor());
			g2.fill(rect);
			Ellipse2D circle = new Ellipse2D.Double();
			circle.setFrame(rect);
			g2.setPaint(balls[i][j].getcColor());
			g2.fill(circle);
		    }
	    }
    }

    /**
     *  MouseHandler -- here is where most of the game is served, since
     *  most of the game depends upon the mouseClicked subclass.
     *  @param m the X-coordinate of the mouse click
     *  @param n the Y-coordinate of the mouse click
     *  @param xball the X-coordinate of the ball selected
     *  @param yball the Y-coordinate of the ball selected
     */
    public class MouseHandler implements MouseListener
    {
	public void mouseClicked(MouseEvent ev)
	{
	    int m = ev.getX();
	    int n = ev.getY();
	
	    int xball = (int)(m / 40);
	    int yball = (int)(n / 40);
	    if (isClickable(xball,yball)) 
		{
		    scount = 0;
		    selectClickable(xball,yball);
		    if (scount > 2) score += ((scount - 2) * (scount - 2));
		}
	    removeSelected();
	    compressDown();
	    compressLeft();
	    if (isClickable(xball,yball)) 
		{
		    selectClickable(xball,yball);
		    highlight();
		}
	    System.out.println("Score: " + score);
	    
	    repaint();
	    checkFinal();
	}

	public void mousePressed(MouseEvent ev)
	{
	}

	public void mouseReleased(MouseEvent ev)
	{
	}

	public void mouseEntered(MouseEvent ev)
	{
	}

	public void mouseExited(MouseEvent ev)
	{
	}
    }
    /**
     *  MouseMotionHandler -- governs the higlighting options int the game,
     *  under mouseMoved.
     */
    public class MouseMotionHandler implements MouseMotionListener
    {
	public void mouseMoved(MouseEvent ev)
	{
	    int m = ev.getX();
	    int n = ev.getY();
	    int xball = (int)(m / 40);
	    int yball = (int)(n / 40);
	    if (isClickable(xball,yball)) 
		selectClickable(xball,yball);
	    highlight();
	    repaint();
		
	}

	public void mouseDragged(MouseEvent ev)
	{
	}
    }

    /**
     *  SDialog -- a dialog box which is called when the game ends.  The 
     *  score is displayed and two buttons are offered, one for a new
     *  game and one for quit and exit.
     */
    class SDialog extends JDialog
    {
	SDialogPanel dialogpanel;
	Container cPane = getContentPane();
	
	public SDialog(JFrame parent)
        {
	    super(parent,"Game Over",true);
	    setSize(300,150);
	    setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	    dialogpanel = new SDialogPanel(this);
	 
	    cPane.add(dialogpanel);
        }
	
	public class SDialogPanel extends JPanel
	{
	    JDialog dialog;
	    
	    JButton newg = new JButton("Try Again");
	    JButton quitg = new JButton("Quit & Exit");
	    
	    SDialogPanel(JDialog dialog)
	    {
		this.dialog = dialog;
		add(new JLabel("Your Final Score: " + score),BorderLayout.CENTER);   
		ButtonHandler b = new ButtonHandler();
		newg.addActionListener(b);
		quitg.addActionListener(b);
		add(newg);
		add(quitg);
	    }

	    class ButtonHandler implements ActionListener
	    {
		public void actionPerformed(ActionEvent ev)
		{
		    if (ev.getSource() == newg)
			{ 
			    dialog.hide();
			    resetBoard();
			}
		    if (ev.getSource() == quitg) System.exit(0);
		}
	    }
	}
    }

    
    /**
     *  checkFinal -- checks the board at the end of every move to see if the 
     *  game is over.  If it is, it will call the dialog box described above.
     *  If all the balls are the background color, it will add 1000 points to
     *  the player's score.
     *  @param isNotFinal tells if the move was the final move possible or not
     *  @param areAllGone asks if absolutley all of the balls are gone, qualifying
     *  the player for the bonus of 1000 points
     */
    public void checkFinal()
    {
	boolean isNotFinal = false;
	boolean areAllGone = true;
	for (int i = 0; i < B_WIDTH; i++)
	    {
		for (int j = 0; j < B_HEIGHT; j++)
		    {
			if (isClickable(i,j)) isNotFinal = true;
			if (balls[i][j].getColor() != BCKGND) areAllGone = false;
		    }
	    }
	if (areAllGone) score += 1000;
	if (!isNotFinal) 
	    {
		System.out.println("Game Over, Final Score: " + score);
		SDialog scorepanel = new SDialog(null);
		scorepanel.show();
	    }

    }

    /** 
     *  isClickable -- returns a boolean value that says whether the piece
     *  selected is "clickable"; int other words, if the pieces around it
     *  are the same color or not.
     *  @param clickable returns whether or not the game ball is 'clickable'
     *  @param home the color of the square
     *  @param north the color of the square above home
     *  @param west the color of the square to the left of home
     *  @param south the color of the square below home
     *  @param east the color of the square to the right of home
     */
    public boolean isClickable(int m, int n)
    {
	boolean clickable = false;
	Color home = balls[m][n].getColor();
	if (home == BCKGND) return (clickable);

	if ((n - 1) >= 0)
	    {
		Color north = balls[m][n - 1].getColor();
		clickable = clickable || (home == north);
	    }
	if ((m - 1) >= 0)
	    {
		Color west = balls[m - 1][n].getColor();
		clickable = clickable || (home == west);
	    }
	if ((n + 1) < B_HEIGHT)
	    {
		Color south = balls[m][n + 1].getColor();
		clickable = clickable || (home == south);
	    } 
	if ((m + 1) < B_WIDTH)
	    {
		Color east = balls[m + 1][n].getColor();
		clickable = clickable || (home == east);
	    }
        return (clickable);
    }

    /**
     *  selectClickable -- a recursive function which searches
     *  the game board for game balls of the same color as the
     *  one selected, and 'selects' them as well.
     */
    public void selectClickable(int m, int n)
    {
	Color home = balls[m][n].getColor();
	balls[m][n].select();	
	scount += 1;
	if ((n - 1) >= 0)
	    {
		Color north = balls[m][n - 1].getColor();
		if ((home.equals(north)) && !(balls[m][n - 1].isSelected())) 
			selectClickable(m,(n - 1));
	    }
	if ((m - 1) >= 0)
	    {
		Color west = balls[m - 1][n].getColor();
		if ((home.equals(west)) && !(balls[m - 1][n].isSelected()))
			selectClickable((m - 1),n);
	    }
	if ((n + 1) < B_HEIGHT)
	    {
		Color south = balls[m][n + 1].getColor();
		if ((home.equals(south)) && !(balls[m][n + 1].isSelected()))
			selectClickable(m,(n + 1));
	    } 
	if ((m + 1) < B_WIDTH)
	    {
		Color east = balls[m + 1][n].getColor();
		if ((home.equals(east)) && !(balls[m + 1][n].isSelected()))
		       selectClickable((m + 1),n);
	    }
    }

    /**
     *  compressLeft -- searches the board to see if an entire column is
     *  the background color (as evidenced with the bottom game ball being
     *  the background color), then advances a next value to find the first
     *  column of non-background color game balls.
     *  @param next the next counter, tracks the non-empty columns
     */
    public void compressLeft()
    {
	int next = 0;
	for (int i = 0; i < B_WIDTH; i++)
	    {
		if (balls[i][(B_HEIGHT - 1)].getColor() != BCKGND)
		    {  
			for (int j2 = 0; j2 < B_HEIGHT; j2++)
			    {  
				balls[next][j2].setColor(balls[i][j2].getColor());
				balls[next][j2].setcColor(balls[i][j2].getcColor());
			    }
			next++;
		    }
	    }
	for (int i3 = next; i3 < B_WIDTH; i3++)
	    {
		for (int j3 = 0; j3 < B_HEIGHT; j3++)
		    {
			balls[i3][j3].setColor(BCKGND);
			balls[i3][j3].setcColor(BCKGND);
		    }
	    }
    }

    /**
     *  highlight -- looks at the board and dehighlights previously highlighted
     *  game balls, and then highlights any selected game balls.  Called from
     *  mouseMoved.
     */
    public void highlight()
    {
	for (int i = 0; i < B_WIDTH; i++)
	    {
		for (int j = 0; j < B_HEIGHT; j++)
		    {
			if (balls[i][j].getColor() == c1)  balls[i][j].setColor(COLOR1);
			if (balls[i][j].getColor() == c2)  balls[i][j].setColor(COLOR2);
			if (balls[i][j].getColor() == c3)  balls[i][j].setColor(COLOR3);
			
			if (balls[i][j].isSelected())
			    {
				if (balls[i][j].getColor() == COLOR1)  balls[i][j].setColor(c1);
				if (balls[i][j].getColor() == COLOR2)  balls[i][j].setColor(c2);
				if (balls[i][j].getColor() == COLOR3)  balls[i][j].setColor(c3);
				balls[i][j].deselect();
			    }
		    }
	    }
    }

    /**
     *  compressDown -- looks at the board and pulls any game pieces down if they
     *  are over any background-colored game balls.  Called from mouseClicked.
     *  @param temp a temp array of Balls, which holds the non-background colored
     *  balls.
     */
    public void compressDown()
    {
	for (int i = 0; i < B_WIDTH; i++)
	    {
		int k = (B_HEIGHT - 1);
		Ball[] temp = new Ball[B_HEIGHT];
		for (int j = (B_HEIGHT - 1); j >= 0; j--)
		    {
			if (balls[i][j].getColor() != BCKGND)
			    {
				temp[k] = new Ball();
				temp[k].setColor(balls[i][j].getColor());
				temp[k].setcColor(balls[i][j].getcColor());
				k--;
			    } 
		    }
		while (k >= 0)
		    {
			temp[k] = new Ball();
			temp[k].setColor(BCKGND);
			temp[k].setcColor(BCKGND);
			k--;
		    }
		for (int l = 0; l < B_HEIGHT; l++)
		    {
			balls[i][l].setColor(temp[l].getColor());
			balls[i][l].setcColor(temp[l].getcColor());
		    }
	    }
    }
    
    /**
     *  removeSelected -- looks at the board and takes all balls that are 
     *  'selected' and turns their color to the background.  Called from
     *  mouseClicked.
     */
    public void removeSelected()
    {
	for (int i = 0; i < B_WIDTH; i++)
	    {
		for (int j = 0; j < B_HEIGHT; j++)
		    {
			if (balls[i][j].isSelected())
			    {
				balls[i][j].setColor(BCKGND);
				balls[i][j].setcColor(BCKGND);
				balls[i][j].deselect();
			    }
		    }
	    }
    }
}

/**
 *  The Object Ball, which extends JPanel.  While JPanel functionality is not int
 *  use, the Ball carries several values including a Color value and a isSelected
 *  boolean value, both of which are manipulated to highlight and remove the game
 *  pieces.
 *  @param bColor the ball color
 *  @param isSelected a boolean flag that selects or deselects a ball from removal
 *  or highlighting during the game
 */
class Ball extends JPanel
{
    private boolean isSelected;
    private Color bColor;
    private Color cColor;

    /**
     *  Ball, the constructor.
     */
    public Ball()
    {
	isSelected = false;
    }

    /**
     *  setColor, sets the color.
     */    
    public void setColor(Color c)
    {
	bColor = c;
    }

    public void setcColor(Color c)
    {
	cColor = c;
    }

    public Color getcColor()
    {
	return cColor;
    }
    /**
     *  getColor, asks for the color of the Ball.
     */
    public Color getColor()
    {
	return bColor;
    }

    /**
     *  isSelected, asks if the Ball is selected or not.
     */
    public boolean isSelected()
    {
	return isSelected;
    }

    /**
     *  select, turns isSelected to true.
     */
    public void select()
    {
	isSelected = true;
    }

    /**
     *  deselect, turns isSelected to false again.
     */
    public void deselect()
    {
	isSelected = false;
    }
}



