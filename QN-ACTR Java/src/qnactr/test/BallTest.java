package qnactr.test;


//http://www.leepoint.net/notes-java/examples/animation/40BouncingBall/bouncingball.html
//Description: Illustrates animation with a ball bouncing in a box
//Possible extensions: faster/slower button,
//Author: Fred Swartz
//Date:   February 2005 ...

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import qnactr.sim.ImageResources;

/////////////////////////////////////////////////////////////// BBDemo
public class BallTest extends JApplet {
  
  //============================================== applet constructor
  public BallTest() {
    add(new BBPanel());
  }
  
  //============================================================ main
  public static void main(String[] args) {
    JFrame win = new JFrame("Bouncing Ball Demo");
    win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    
    win.setContentPane(new BBPanel());
    
    win.pack();
    win.setVisible(true); 
  }
}//endclass BBDemo

/////////////////////////////////////////////////////////////////// BBPanel
class BBPanel extends JPanel {
  BallInBox m_bb;   // The bouncing ball panel
  
  //========================================================== constructor
  /** Creates a panel with the controls and bouncing ball display. */
  BBPanel() {
    //... Create components
    m_bb = new BallInBox();        
    JButton startButton = new JButton("Start");        
    JButton stopButton  = new JButton("Stop");
    
    //... Add Listeners
    startButton.addActionListener(new StartAction());
    stopButton.addActionListener(new StopAction());
    
    //... Layout inner panel with two buttons horizontally
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new FlowLayout());
    buttonPanel.add(startButton);
    buttonPanel.add(stopButton);
    
    //... Layout outer panel with button panel above bouncing ball
    this.setLayout(new BorderLayout());
    this.add(buttonPanel, BorderLayout.NORTH);
    this.add(m_bb       , BorderLayout.CENTER);
  }//end constructor
  
  
  ////////////////////////////////////// inner listener class StartAction
  class StartAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      m_bb.setAnimation(true);
    }
  }
  
  
  //////////////////////////////////////// inner listener class StopAction
  class StopAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      m_bb.setAnimation(false);
    }
  }
  
  
  /////////////////////////////////////////////////////////////// BouncingBall
  public class BallInBox extends JPanel {
    //============================================== fields
    //... Instance variables representing the ball.
    private Ball m_ball         = new Ball(0, 0, 2, 3);
    
    //... Instance variables for the animiation
    private int   m_interval  = 35;  // Milliseconds between updates.
    private Timer m_timer;           // Timer fires to anmimate one step.
    
    //========================================================== constructor
    /** Set panel size and creates timer. */
    public BallInBox() {
      setPreferredSize(new Dimension(200, 80));
      setBorder(BorderFactory.createLineBorder(Color.BLACK));
      m_timer = new Timer(m_interval, new TimerAction());
    }
    
    //========================================================= setAnimation
    /** Turn animation on or off.
     *@param turnOnOff Specifies state of animation.
     */
    public void setAnimation(boolean turnOnOff) {
      if (turnOnOff) {
        m_timer.start();  // start animation by starting the timer.
      } else {
        m_timer.stop();   // stop timer
      }
    }
    
    //======================================================= paintComponent
    public void paintComponent(Graphics g) {
      super.paintComponent(g);  // Paint background, border
      m_ball.draw(g);           // Draw the ball.
    }
    
    //////////////////////////////////// inner listener class ActionListener
    class TimerAction implements ActionListener {
      //================================================== actionPerformed
      /** ActionListener of the timer.  Each time this is called,
       *  the ball's position is updated, creating the appearance of
       *  movement.
       *@param e This ActionEvent parameter is unused.
       */
      public void actionPerformed(ActionEvent e) {
        m_ball.setBounds(getWidth(), getHeight());
        m_ball.move();  // Move the ball.
        repaint();      // Repaint indirectly calls paintComponent.
      }
    }
  }//endclass
  
  
  ///////////////////////////////////////////////////////////////// BallModel
  public class Ball {
    //... Constants
    final static int DIAMETER = 21;
    
    //... Instance variables
    private int m_x;           // x and y coordinates upper left
    private int m_y;
    
    private int m_velocityX;   // Pixels to move each time move() is called.
    private int m_velocityY;
    
    private int m_rightBound;  // Maximum permissible x, y values.
    private int m_bottomBound;
    
    //======================================================== constructor
    public Ball(int x, int y, int velocityX, int velocityY) {
      m_x = x;
      m_y = y;
      m_velocityX = velocityX;
      m_velocityY = velocityY;
    }
    
    //======================================================== setBounds
    public void setBounds(int width, int height) {
      m_rightBound  = width  - DIAMETER;
      m_bottomBound = height - DIAMETER;
    }
    
    //============================================================== move
    public void move() {
      //... Move the ball at the give velocity.
      m_x += m_velocityX;
      m_y += m_velocityY;        
      
      //... Bounce the ball off the walls if necessary.
      if (m_x < 0) {                  // If at or beyond left side
        m_x         = 0;            // Place against edge and
        m_velocityX = -m_velocityX; // reverse direction.
        
      } else if (m_x > m_rightBound) { // If at or beyond right side
        m_x         = m_rightBound;    // Place against right edge.
        m_velocityX = -m_velocityX;  // Reverse direction.
      }
      
      if (m_y < 0) {                 // if we're at top
        m_y       = 0;
        m_velocityY = -m_velocityY;
        
      } else if (m_y > m_bottomBound) { // if we're at bottom
        m_y       =  m_bottomBound;
        m_velocityY = -m_velocityY;
      }
    }
    
    //============================================================== draw
    public void draw(Graphics g) {
      //g.fillOval(m_x, m_y, DIAMETER, DIAMETER);
      
//      g.setColor(Color.BLUE);
//      g.drawLine(m_x, m_y, m_x + DIAMETER, m_y + DIAMETER);
      
//      g.drawString("Test", m_x, m_y);
      
      g.drawOval(m_x, m_y, DIAMETER, DIAMETER);
      
      //((Graphics2D)g).drawImage(ImageResources.biVisual, m_x, m_y, null);
      
    }
    
    //============================================= getDiameter, getX, getY
    public int  getDiameter() { return DIAMETER;}
    public int  getX()        { return m_x;}
    public int  getY()        { return m_y;}
    
    //======================================================== setPosition
    public void setPosition(int x, int y) {
      m_x = x;
      m_y = y;
    }
  }
  
}//endclass BBPanel
