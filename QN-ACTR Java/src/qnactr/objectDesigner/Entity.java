/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;

import java.util.*;

import qnactr.sim.QnactrSimulation;

import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.simEngine.SimSystem;

public class Entity {
  
  public Chunk Chunk;
  public int Color;
  public double Direct_Cast_Delay;
  public double Duration; // used in temporal module
  public double creationClockTime;
  public String Entity_Type = "";
  public int Event_Priority;
  public boolean Filled;
  public String From = "";
  public int Group;
  public String ID;
  public String Note = "";
  public Production_Rule Obsolete_Production_Rule_Clone_From_Matching_And_Selection_To_Execution;
  public LinkedList<Production_Rule> Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution= new LinkedList<Production_Rule>();
  public Object Response_Item = new Object();
  public LinkedList <Production_Rule> Rule_Pool = new LinkedList<Production_Rule>();
  public double Scheduled_Task_Enter_Clock_Time;
  public LinkedList<String> The_Chunk_Spec_Change_List= new LinkedList<String>();
  public double Time;
  public boolean Time_Computed;
  public String To  = "";
  public boolean Trash;
  public int Tag = 0;
  public String hmiID = ""; // start from 1
  
  public Job ownerJob;
  
  public Enums.EntityPlaceHeader currentPlaceHeader = Enums.EntityPlaceHeader.none;  // in or left
  public Enums.ServerName currentServerNodeLocalName = Enums.ServerName.none; // lowercase no space, e.g., "visualmodule"
  public Enums.NodeSection currentServerSectionName = Enums.NodeSection.none; // lowercase no space, e.g., "input" , "service", "output"
  
  public Entity() {
    Chunk = new Chunk();
    Color = 0;
    Direct_Cast_Delay = 0.0;
    Duration = 0.0;
    creationClockTime = SimSystem.clock();
    Event_Priority = 0;
    Filled = false;
    Group = 0;
    ID = "0";
    Rule_Pool = null;
    Scheduled_Task_Enter_Clock_Time = 0.0;
    Time = 0.0;
    Time_Computed = false;
    Trash = false;
    
    Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
    QnactrSimulation.entityNumber++;
    
  }

  /**
   * will create a new entity with new tag
   * @return
   */
  public Entity cloneWithNewTag(){
    
    QnactrSimulation sim = SimSystem.qnactrSims [Integer.valueOf(this.hmiID) - 1 ];
    
    Entity returnEntity = new Entity();
    
    QnactrSimulation.globalAllEntitiesListAddLast(returnEntity);
    
    returnEntity.Chunk = sim.funs.ChunkFun__Chunk_Clone(this.Chunk); 
    returnEntity.Color = this.Color;
    returnEntity.Direct_Cast_Delay = this.Direct_Cast_Delay;
    returnEntity.Duration = this.Duration;
//    returnEntity.creationClockTime = this.creationClockTime;
    returnEntity.Entity_Type = this.Entity_Type;
    returnEntity.Event_Priority = this.Event_Priority;
    returnEntity.Filled = this.Filled;
    returnEntity.From = this.From;
    returnEntity.Group = this.Group;
    returnEntity.hmiID = this.hmiID;
    returnEntity.ID = this.ID;
    returnEntity.Note = this.Note;
    returnEntity.Obsolete_Production_Rule_Clone_From_Matching_And_Selection_To_Execution = this.Obsolete_Production_Rule_Clone_From_Matching_And_Selection_To_Execution;
    returnEntity.ownerJob = this.ownerJob; 
    
    if(this.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution != null)
      returnEntity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution = (LinkedList<Production_Rule>) this.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.clone();
    
    returnEntity.Response_Item = this.Response_Item;
    
    if(this.Rule_Pool != null)
      returnEntity.Rule_Pool = (LinkedList<Production_Rule>) this.Rule_Pool.clone();
    
    returnEntity.Scheduled_Task_Enter_Clock_Time = this.Scheduled_Task_Enter_Clock_Time;
    
    
    //returnEntity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
    //QnactrSimulation.entityNumber++; //moved to new Entity()
        
    returnEntity.The_Chunk_Spec_Change_List = this.The_Chunk_Spec_Change_List ;
    returnEntity.Time = this.Time;
    returnEntity.Time_Computed = this.Time_Computed;
    returnEntity.To = this.To;
    returnEntity.Trash = this.Trash;
    
    returnEntity.currentPlaceHeader = this.currentPlaceHeader;
    returnEntity.currentServerNodeLocalName = this.currentServerNodeLocalName;
    returnEntity.currentServerSectionName = this.currentServerSectionName;
    
    return  returnEntity;
  }
  
  
public void copyPropertiesOtherThanTagTo(Entity returnEntity){
    
    QnactrSimulation sim = SimSystem.qnactrSims [Integer.valueOf(this.hmiID) - 1 ];
    
    returnEntity.Chunk = sim.funs.ChunkFun__Chunk_Clone(this.Chunk); 
    returnEntity.Color = this.Color;
    returnEntity.Direct_Cast_Delay = this.Direct_Cast_Delay;
    returnEntity.Duration = this.Duration;
//    returnEntity.creationClockTime = this.creationClockTime;
    returnEntity.Entity_Type = this.Entity_Type;
    returnEntity.Event_Priority = this.Event_Priority;
    returnEntity.Filled = this.Filled;
    returnEntity.From = this.From;
    returnEntity.Group = this.Group;
    returnEntity.hmiID = this.hmiID;
    returnEntity.ID = this.ID;
    returnEntity.Note = this.Note;
    returnEntity.Obsolete_Production_Rule_Clone_From_Matching_And_Selection_To_Execution = this.Obsolete_Production_Rule_Clone_From_Matching_And_Selection_To_Execution;
    returnEntity.ownerJob = this.ownerJob; 
    
    if(this.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution != null)
      returnEntity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution = (LinkedList<Production_Rule>) this.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.clone();
    
    returnEntity.Response_Item = this.Response_Item;
    
    if(this.Rule_Pool != null)
      returnEntity.Rule_Pool = (LinkedList<Production_Rule>) this.Rule_Pool.clone();
    
    returnEntity.Scheduled_Task_Enter_Clock_Time = this.Scheduled_Task_Enter_Clock_Time;
        
    //returnEntity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
    //QnactrSimulation.entityNumber++; //moved to new Entity()
        
    returnEntity.The_Chunk_Spec_Change_List = this.The_Chunk_Spec_Change_List ;
    returnEntity.Time = this.Time;
    returnEntity.Time_Computed = this.Time_Computed;
    returnEntity.To = this.To;
    returnEntity.Trash = this.Trash;
    
  }
  
  /**
   * if not change, use null as input.
   * @param CurrentPlaceHeader
   * @param CurrentServerNodeLocalName
   * @param CurrentServerSectionName
   */
  public void updateCurrentPlace(Enums.EntityPlaceHeader CurrentPlaceHeader, Enums.ServerName CurrentServerNodeLocalName, Enums.NodeSection CurrentServerSectionName){
    
    if(CurrentPlaceHeader != null) currentPlaceHeader = CurrentPlaceHeader;
    if(CurrentServerNodeLocalName != null) currentServerNodeLocalName = CurrentServerNodeLocalName;
    if(CurrentServerSectionName != null) currentServerSectionName = CurrentServerSectionName;
    
    //update info about this entity in EntityViewer's data table, so that the GUI will update.
    if(QnactrSimulation.entitiesViewerEnable)QnactrSimulation.newContentPaneEntitiesViewer.model.updateAnEntityInModelData(this);
    
    
//    System.out.println("Entity.updateCurrentPlace " + currentPlaceHeader + " " + currentServerNodeLocalName + " " + currentServerSectionName);
  }
  
  public double getDurationSinceCreation (){
    return SimSystem.clock() - this.creationClockTime;
  }
  
}
