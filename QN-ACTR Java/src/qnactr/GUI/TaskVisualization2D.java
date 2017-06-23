package qnactr.GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.lang.reflect.Field;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map.Entry;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

import qnactr.sim.GlobalUtilities;
import qnactr.sim.ImageResources;
import qnactr.sim.QnactrSimulation;



public class TaskVisualization2D extends JPanel 
{
  public int objectCounter = 0;
  public Hashtable<String, Object> currentAllObjects = new Hashtable<String, Object>();
  public Hashtable<String, DynamicVisualObjects> currentDynamicObjects = new Hashtable<String, DynamicVisualObjects>();
  public String[] leftHandFingerIDs = new String[5]; // 0, "thumb"; 1, "index"; 2, "middle"; 3, "ring"; 4, "pinkie"
  public String[] rightHandFingerIDs = new String[5];
  public String mouseCursorID;
  public String visualAttentionCircleID;
  public String vocalResponseDisplayID;
  
  public int defaultWidthPerChar = 7;
  public int defaultHeightPerChar = 11;
  
  private int winX1 = 30; //upper left corner
  private int winY1 = 30; //upper left corner
  private int winX2 = winX1 + QnactrSimulation.simulatedWindowDefaultSizeX; //bottom right corner
  private int winY2 = winY1 + QnactrSimulation.simulatedWindowDefaultSizeY; //bottom right corner
  
  
  
  public TaskVisualization2D(){
    
    
    setBackground(Color.WHITE);
    setLayout(null);
    
    //add simulated window corners
    createStaticText("(" + 0 + ", " + 0 + ")    Visual Display", winX1 , winY1 - 20);
    createStaticText("(" + (winX2 - winX1) + ", " + (winY2 - winY1) + ")", winX2 , winY2);
    
    createStaticText("Audio Display", 0 , winY2 + 60);
    createStaticText("Vocal Response", 180 , winY2 + 60);
    createStaticText("Manual Response", 180 + 290, winY2 + 60 + 110);
    leftHandFingerIDs[0] = createStaticText("--", 180 + 260, winY2 + 60 + 90);
    leftHandFingerIDs[1] = createStaticText("--", 180 + 260 - 10, winY2 + 60 + 90 - 20);
    leftHandFingerIDs[2] = createStaticText("--", 180 + 260 - 35, winY2 + 60 + 90 - 40);
    leftHandFingerIDs[3] = createStaticText("--", 180 + 260 - 60, winY2 + 60 + 90 - 20);
    leftHandFingerIDs[4] = createStaticText("--", 180 + 260 - 80, winY2 + 60 + 90);
    rightHandFingerIDs[0] = createStaticText("--", 180 + 390 - 10, winY2 + 60 + 90);
    rightHandFingerIDs[1] = createStaticText("--", 180 + 390 + 10, winY2 + 60 + 90 - 20);
    rightHandFingerIDs[2] = createStaticText("--", 180 + 390 + 35, winY2 + 60 + 90 - 40);
    rightHandFingerIDs[3] = createStaticText("--", 180 + 390 + 60, winY2 + 60 + 90 - 20);
    rightHandFingerIDs[4] = createStaticText("--", 180 + 390 + 70, winY2 + 60 + 90);
    mouseCursorID = createDynamicImage(ImageResources.biMouseCursor, 0, 0, 10, 16); 
    visualAttentionCircleID = createDynamicOval (0, 0, 20, 20, Color.red); 
    vocalResponseDisplayID = createStaticText("--", 180, winY2 + 60 + 20);
    
    //test
    //    String id_1 = createStaticText("test1", 150, 200);
    //    setStaticTextString(rightHandFingerIDs[3], "updated text");
    //    removeObject(rightHandFingerIDs[0]);
    //    setDynamicObjectLocation(mouseCursorID, 100, 50);
    //createDynamicText("H", 100, 100);
    //createStaticText("createStaticText", winX1, winY1);
    
  }
  
  //================= paintComponent
  public void paintComponent(Graphics g) {
    super.paintComponent(g);  // Paint background, border
    
    //draw simulated window border lines
    g.drawLine(winX1, winY1, winX2, winY1);
    g.drawLine(winX1, winY1, winX1, winY2);
    g.drawLine(winX1, winY2, winX2, winY2);
    g.drawLine(winX2, winY1, winX2, winY2);
    
    //draw TaskVisualization2D default images
    g.drawImage(ImageResources.biVisual, 0, 0, 30, 30, null);
    g.drawImage(ImageResources.biAudio, 0, winY2 + 30, 30, 24, null);
    g.drawImage(ImageResources.biVocal, 180, winY2 + 30, 30, 30, null);
    g.drawImage(ImageResources.biLeftHand, 180 + 220, winY2 + 30 + 120, 24, 32, null);
    g.drawImage(ImageResources.biRightHand, 180 + 220 + 200, winY2 + 30 + 120, 24, 32, null);
    
    //draw each object in currentDynamicObjects using its own draw method
    Iterator<Entry<String, DynamicVisualObjects>> itrEntires = currentDynamicObjects.entrySet().iterator();
    while(itrEntires.hasNext()){
      Entry<String, DynamicVisualObjects> anEntry = itrEntires.next();
      DynamicVisualObjects anObj = anEntry.getValue();
      if(!anObj.hide)anObj.draw(g);
    }
    
    //test
    //g.drawLine(100 + winX1, 100 + winY1, 100 + winX1 + 7, 100 + winY1 + 11);
  }
  
  
  
  //========= static (do not move them) objects
  
  
  private class TV2DLabel extends JLabel 
  {
    String ID;
    
    //    public TV2DLabel (String text) {
    //      super(text);
    //      setLocation(0, 0);
    //      setSize(this.preferredSize());
    //      objectCounter++;
    //      ID = String.valueOf(objectCounter);
    //    }
    
    public TV2DLabel (String text, int locX, int locY) {
      super(text);
      setLocation(locX, locY);
      //setSize(this.preferredSize());
      setSize(new Dimension(text.length() * (defaultWidthPerChar + 2), defaultHeightPerChar + 2));
      objectCounter++;
      ID = String.valueOf(objectCounter);
      currentAllObjects.put(ID, this);
    }
    
  }
  
  /**
   * 
   * @param text
   * @param locX, reference to the TaskVisualization2D, upper-left corner
   * @param locY, reference to the TaskVisualization2D, upper-left corner
   * @return
   */
  public String createStaticText(String text, int locX, int locY){
    TV2DLabel label = new TV2DLabel (text, locX, locY);
    add(label);
    return label.ID;
  }
  
  
  /**
   * 
   * @param ID
   * @param newText
   */
  public void setStaticTextString (String ID, String newText){
    TV2DLabel label = (TV2DLabel)currentAllObjects.get(ID);
    label.setText(newText);
    label.setSize(new Dimension(newText.length() * (defaultWidthPerChar + 2), defaultHeightPerChar + 2));
  }
  
  public void setStaticTextBackgroundColor(String ID, Color color){
    TV2DLabel label = (TV2DLabel)currentAllObjects.get(ID);
    label.setBackground(color);
    label.setOpaque(true);
  }
  
  
  //========== dynamic objects (can move them)
  
  
  private abstract class DynamicVisualObjects {
    int locX; //reference to simulated window (0,0) 
    int locY; //reference to simulated window (0,0)
    String ID;
    Color color = Color.BLACK;
    
    boolean hide = false;
    
    public DynamicVisualObjects() {
      objectCounter++;
      ID = String.valueOf(objectCounter);
      currentAllObjects.put(ID, this);
      currentDynamicObjects.put(ID, this);
    }
    
    public abstract void draw (Graphics g);
  }
  
  private class DynamicText extends DynamicVisualObjects{
    String text;
    
    public DynamicText(String t, int x, int y){
      text = t;
      locX = x;
      locY = y;
    }
    
    public void draw (Graphics g){
      g.setColor(color);
      int heightOffSet = (int) (g.getFontMetrics().getHeight()  / 2); //g.drawString seems to use the bottom left corner of the text as the reference point.
      g.drawString(text, locX + winX1, locY + winY1 + heightOffSet);  //because reference to simulated window (0,0)
    }
    
  }
  
  private class DynamicImage extends DynamicVisualObjects{
    int displayHeight;
    int displayWidth;
    BufferedImage srcImage;
    
    public DynamicImage(BufferedImage bi, int x, int y, int w, int h){
      srcImage = bi;
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;

    }
    
    public void draw (Graphics g){
      g.drawImage(srcImage, locX + winX1, locY + winY1, displayWidth, displayHeight, null);  //because reference to simulated window (0,0)
    }
    
  }
  
  private class DynamicRect extends DynamicVisualObjects{
    int displayWidth;
    int displayHeight;
    
    public DynamicRect(int x, int y, int w, int h){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;

    }
    
    public DynamicRect(int x, int y, int w, int h, Color cl){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;
      color = cl;

    }
    
    public void draw (Graphics g){
      g.setColor(color); 
      g.drawRect(locX + winX1, locY + winY1, displayWidth, displayHeight);  //because reference to simulated window (0,0)
    }
    
  }
  
  private class DynamicOval extends DynamicVisualObjects{
    int displayHeight;
    int displayWidth;
    
    public DynamicOval(int x, int y, int w, int h){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;

    }
    
    public DynamicOval(int x, int y, int w, int h, Color cl){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;
      color = cl;

    }
    
    public void draw (Graphics g){
      g.setColor(color); 
      g.drawOval(locX + winX1, locY + winY1, displayWidth, displayHeight);  //because reference to simulated window (0,0)
    }
    
  }
    
  private class DynamicLine extends DynamicVisualObjects{
    int displayWidth;
    int displayHeight;
    
    public DynamicLine(int x, int y, int w, int h){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;

    }
    
    public DynamicLine(int x, int y, int w, int h, Color cl){
      locX = x;
      locY = y;
      displayWidth = w;
      displayHeight = h;
      color = cl;

    }
    
    public void draw (Graphics g){
      g.setColor(color); 
      g.drawLine(locX + winX1, locY + winY1, locX + winX1 + displayWidth, locY + winY1 + displayHeight);  //because reference to simulated window (0,0)
    }
    
  }
  
  //============= create and set properties of dynamic objects
  
  /**
   * 
   * @param text
   * @param x, upper-left corner, reference to simulated window (0,0)
   * @param y, upper-left corner, reference to simulated window (0,0)
   * @return
   */
  public String createDynamicText (String text, int x, int y){
    DynamicText dt = new DynamicText (text, x, y);
    repaint();
    //System.out.println("TaskVisualization2D createDynamicText text: " + text);
    return dt.ID;
  }
  
  /**
   * 
   * @param bi
   * @param x, upper-left corner, reference to simulated window (0,0)
   * @param y, upper-left corner, reference to simulated window (0,0)
   * @param w, display width
   * @param h, display height
   * @return
   */
  public String createDynamicImage (BufferedImage bi, int x, int y, int w, int h){
    DynamicImage di = new DynamicImage (bi, x, y, w, h);
    repaint();
    //System.out.println("TaskVisualization2D createDynamicImage ");
    return di.ID;
  }
  
  public String createDynamicLine (int x, int y, int w, int h){
    DynamicLine dl = new DynamicLine (x, y, w, h);
    repaint();
    return dl.ID;
  }
  
  public String createDynamicLine (int x, int y, int w, int h, Color color){
    DynamicLine dl = new DynamicLine (x, y, w, h, color);
    repaint();
    return dl.ID;
  }
  
  
  public String createDynamicOval (int x, int y, int w, int h){
    DynamicOval dyo = new DynamicOval (x, y, w, h);
    repaint();
    return dyo.ID;
  }
  
  public String createDynamicOval (int x, int y, int w, int h, Color color){
    DynamicOval dyo = new DynamicOval (x, y, w, h, color);
    repaint();
    return dyo.ID;
  }
  
  public String createDynamicRect (int x, int y, int w, int h){
    DynamicRect drect = new DynamicRect (x, y, w, h);
    repaint();
    return drect.ID;
  }
  
  public String createDynamicRect (int x, int y, int w, int h, Color color){
    DynamicRect drect = new DynamicRect (x, y, w, h, color);
    repaint();
    return drect.ID;
  }
  
  public void setDynamicTextColor (String ID, String colorString){
    if(!currentDynamicObjects.containsKey(ID)){
      System.out.println("ERROR! TaskVisualization2D.setDynamicTextColor has non-existing currentDynamicObjects ID: " + ID);
      return;
    }
    
    DynamicText dt = (DynamicText)currentDynamicObjects.get(ID);
    dt.color = GlobalUtilities.colorStringToColorType(colorString);    
    repaint();
  }
  
  public void setDynamicObjectLocation (String ID, int x, int y){
    if(!currentDynamicObjects.containsKey(ID)){
      System.out.println("ERROR! TaskVisualization2D.setDynamicObjectLocation has non-existing currentDynamicObjects ID: " + ID);
      return;
    }
    currentDynamicObjects.get(ID).locX = x;
    currentDynamicObjects.get(ID).locY = y;
    repaint();
  }
  
  //============= general methods for all Objects   
  
  public void removeObject(String ID){
    if(!currentAllObjects.containsKey(ID)){
      System.out.println("ERROR! TaskVisualization2D.removeObject has non-existing currentAllObjects ID: " + ID);
      return;
    }
    
    if(currentAllObjects.get(ID) instanceof TV2DLabel){
      this.remove((Component)currentAllObjects.get(ID));
    }
    else{ //all dynamic objects
      if(!currentDynamicObjects.containsKey(ID)){
        System.out.println("ERROR! TaskVisualization2D.removeObject has non-existing currentDynamicObjects ID: " + ID);
        return;
      }
      currentDynamicObjects.remove(ID);
    }
    repaint();
  }
  
  public void hideObject(String ID){
    if(!currentAllObjects.containsKey(ID)){
      System.out.println("ERROR! TaskVisualization2D.hideObject has non-existing currentAllObjects ID: " + ID);
      return;
    }
    
    if(currentAllObjects.get(ID) instanceof TV2DLabel){
      ((TV2DLabel)currentAllObjects.get(ID)).hide();
    }
    else{ //all dynamic objects
      if(!currentDynamicObjects.containsKey(ID)){
        System.out.println("ERROR! TaskVisualization2D.hideObject has non-existing currentDynamicObjects ID: " + ID);
        return;
      }
      currentDynamicObjects.get(ID).hide = true;
    }
    
    repaint();
  }
  
  public void showObject(String ID){
    if(!currentAllObjects.containsKey(ID)){
      System.out.println("ERROR! TaskVisualization2D.showObject has non-existing currentAllObjects ID: " + ID);
      return;
    }
    
    if(currentAllObjects.get(ID) instanceof TV2DLabel){
      ((TV2DLabel)currentAllObjects.get(ID)).show();
    }
    else{ //all dynamic objects
      if(!currentDynamicObjects.containsKey(ID)){
        System.out.println("ERROR! TaskVisualization2D.showObject has non-existing currentDynamicObjects ID: " + ID);
        return;
      }
      currentDynamicObjects.get(ID).hide = false;
    }
    
    repaint();
  }
}
