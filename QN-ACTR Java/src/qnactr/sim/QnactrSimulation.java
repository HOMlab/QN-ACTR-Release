/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.sim;

import java.awt.Color;
import java.awt.Dimension;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JFrame;

import qnactr.GUI.EntitiesViewer;
import qnactr.GUI.TaskVisualization2D;
import qnactr.GUI.TaskVisualization3D;
import qnactr.objectDesigner.Entity;
//import qnactr.taskInterface.gui.*;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.simEngine.HybridEventQueue;
import jmt.engine.simEngine.SimEvent;
import jmt.engine.simEngine.SimSystem;
import jmt.engine.simEngine.Simulation;
import jmt.engine.simEngine.Simulation.SimNode;

/**
 * each QnactrSimulation object represents an HMI unit
 * @author shicao
 *
 */
public class QnactrSimulation
{
  /**
   * start from 1. Operator ID. defined by # of Regions in JMT network drawing
   */
  public int ID;  
  public Simulation simJMT;
  
  
  public Variables vars = new Variables(this);
  public Functions funs = new Functions(this);
  public ServerLogics logics = new ServerLogics(this);
  public QnactrFiles files = new QnactrFiles(this);
  public PathLogics pathLogics = new PathLogics(this);

  
  public static boolean globalVarInitialized = false;
  public static Hashtable<String, NetNode> globalAllNetNodesTable;
  
  public static LinkedList<Entity> globalAllEntitiesList = new LinkedList<Entity>();
  public static int entityNumber = 1;
  
  public static JFrame frameEntitiesViewer;
  public static EntitiesViewer newContentPaneEntitiesViewer;

  public static JFrame frameTaskVisualization2DViewer;
  public static TaskVisualization2D taskVisualization2D; //currently just one static member may change this to each object has one member
  
  //public TaskInterfaceWindow ucWindow;
  
  public static JFrame frameTaskVisualization3DViewer;
  public static TaskVisualization3D taskVisualization3D; 
  
  final public static int simulatedWindowDefaultSizeX = 1350;  
  final public static int simulatedWindowDefaultSizeY = 750;
  final public static int taskVisualization2DExtendSizeX = 120;
  final public static int taskVisualization2DExtendSizeY = 280;
  
  final public static int taskVisualization3DSizeX = 800;
  final public static int taskVisualization3DSizeY = 600;
  
  public static double simSleepCycle = 0.5; // second of real world time
  public static double simLastShownClock;
  
  public static double simStartRealClockTime; // Millisecond
  public static double simEndRealClockTime; // Millisecond
  public static double simEndSimulationClockTime;
  
  
  ///////////// SETUP Begin///////////////////
  public static boolean entitiesViewerEnable = false; // true or false
  public static boolean taskVisualization2DEnable = false;
  public static boolean taskVisualization3DEnable = false;
  public static boolean taskInterfaceWindowEnable = false;
    
  public static double simSpeedFactor = -1; // -1 or any number < 0 means as fast as possible, larger number means faster simulation. N times normal speed.
  public static boolean computeUtilization = true;
  
  ///////////// SETUP end ////////////////////
  
  
  
  



  
  public QnactrSimulation (int id, Simulation simulation){
    
    ID = id; 
    simJMT= simulation; 
    
    //special initialization and re-initialization for static members
    if(!globalVarInitialized){
      globalAllNetNodesTable = new Hashtable<String, NetNode>();
      List<SimNode> nodes = simJMT.getSimNodes();
      
      for (int i = 0; i < nodes.size(); i++) {
        NetNode aNetNode = (nodes.get(i)).getNode();
        String lowerCaseNoSpaceName =  GlobalUtilities.stringLowNoSpace(aNetNode.getName());
        globalAllNetNodesTable.put(lowerCaseNoSpaceName, aNetNode);
      }
      
      //reset these before each simulation run
      globalAllEntitiesList = new LinkedList<Entity>();
      entityNumber = 1;
      simLastShownClock = 0.0;
      
      globalVarInitialized = true;
    }
    
  }

  
  public static void globalAllEntitiesListAddLast(Entity anEntity){
    
    QnactrSimulation.globalAllEntitiesList.addLast( anEntity );
    
    if(entitiesViewerEnable){
      //here update EntitiesViewer's data model
        newContentPaneEntitiesViewer.updateModelData(QnactrSimulation.globalAllEntitiesList);
    }
  }
  
  public static void createAndShowEntitiesViewerGUI() {
    
    frameEntitiesViewer = new JFrame("EntitiesViewer");
    frameEntitiesViewer.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);

    newContentPaneEntitiesViewer = new EntitiesViewer();
    newContentPaneEntitiesViewer.setOpaque(true); //content panes must be opaque
    frameEntitiesViewer.setContentPane(newContentPaneEntitiesViewer);

    frameEntitiesViewer.pack();
    frameEntitiesViewer.setLocationByPlatform(true);
    frameEntitiesViewer.setVisible(true);
}
  
  public void createAndShowTaskInterfaceWindowGUI() {
//	  ucWindow=new TaskInterfaceWindow(this);
//	  ucWindow.setVisible(true);
  }
  
  public static void createAndShowTaskVisualization2DViewerGUI() {
    frameTaskVisualization2DViewer  = new JFrame("TaskVisualization2DViewer");
    frameTaskVisualization2DViewer.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    
    taskVisualization2D = new TaskVisualization2D();
    taskVisualization2D.setOpaque(true); //content panes must be opaque
    frameTaskVisualization2DViewer.setContentPane(taskVisualization2D);

    frameTaskVisualization2DViewer.pack();
    frameTaskVisualization2DViewer.setLocationByPlatform(true);
    frameTaskVisualization2DViewer.setVisible(true);
    
    int x = simulatedWindowDefaultSizeX + taskVisualization2DExtendSizeX;
    int y = simulatedWindowDefaultSizeY + taskVisualization2DExtendSizeY;
    
    frameTaskVisualization2DViewer.setSize(x, y);
    frameTaskVisualization2DViewer.setBackground(Color.WHITE);
  }
  
  public static void createAndShowTaskVisualization3DViewerGUI() {
    frameTaskVisualization3DViewer  = new JFrame("TaskVisualization3DViewer");
    frameTaskVisualization3DViewer.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    
    taskVisualization3D = new TaskVisualization3D();
    taskVisualization3D.setOpaque(true); //content panes must be opaque
    frameTaskVisualization3DViewer.setContentPane(taskVisualization3D);

    frameTaskVisualization3DViewer.pack();
    frameTaskVisualization3DViewer.setLocationByPlatform(true);
    frameTaskVisualization3DViewer.setVisible(true);
    
    int x = taskVisualization3DSizeX;
    int y = taskVisualization3DSizeY;
    
    frameTaskVisualization3DViewer.setSize(x, y);
    frameTaskVisualization3DViewer.setBackground(Color.WHITE);
  }
  
  
  public static LinkedList<Entity> getAllEntitiesCarriedBySimEventsAtCurrentClock(){
    LinkedList<Entity> returnList = new LinkedList<Entity>();
    LinkedList<Integer> entityTags = new LinkedList<Integer>();
    
    Iterator<SimEvent> itrEvents = ((HybridEventQueue)SimSystem.getFutureQueue()).getCurrentList().iterator();
    while(itrEvents.hasNext()){
      SimEvent anEvent = itrEvents.next();
      if(anEvent.eventTime() == SimSystem.clock()){
        Job job = (Job)anEvent.getData();
        Entity entity = job.qnactrEntity;
        if(!entityTags.contains(entity.Tag)){
          entityTags.addLast(entity.Tag);
          returnList.addLast(entity);
        }
      }
      
    }
    
    return returnList;
  }
  
  public static LinkedList<Entity> getAllEntitiesCarriedBySimEventsAtCurrentClockNoTrash(){
    LinkedList<Entity> returnList = new LinkedList<Entity>();
    LinkedList<Integer> entityTags = new LinkedList<Integer>();
    
    Iterator<SimEvent> itrEvents = ((HybridEventQueue)SimSystem.getFutureQueue()).getCurrentList().iterator();
    while(itrEvents.hasNext()){
      SimEvent anEvent = itrEvents.next();
      if(anEvent.eventTime() == SimSystem.clock()){
        Job job = (Job)anEvent.getData();
        
        if(job == null) 
        	continue;
        
        Entity entity = job.qnactrEntity;
        if(!entityTags.contains(entity.Tag) && !entity.Trash){
          entityTags.addLast(entity.Tag);
          returnList.addLast(entity);
        }
      }
      
    }
    
    return returnList;
  }
  
  public static LinkedList<Entity> getNotEndedGlobalAllEntitiesList(){
    LinkedList<Entity> returnList = new LinkedList<Entity>();
    for(Entity anEntity : globalAllEntitiesList){
      if(!anEntity.Trash) returnList.addLast(anEntity);
    }
    return returnList;
  }
  
}
