/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.sim;

import qnactr.objectDesigner.*;

import java.util.*;
import java.util.Map.Entry;

import javax.media.j3d.Transform3D;
import javax.swing.JOptionPane;
import javax.vecmath.Vector3f;

import org.apache.poi.ss.usermodel.Sheet;

import jmt.common.exception.NetException;
import jmt.engine.NodeSections.Queue;
import jmt.engine.NodeSections.Server;
import jmt.engine.QueueNet.Job;
import jmt.engine.QueueNet.JobInfo;
import jmt.engine.QueueNet.JobInfoList;
import jmt.engine.QueueNet.LinkedJobInfoList;
import jmt.engine.QueueNet.NetEvent;
import jmt.engine.QueueNet.NetNode;
import jmt.engine.QueueNet.NodeSection;
import jmt.engine.simEngine.SimSystem;
import jmt.engine.simEngine.RemoveToken;

public class ServerLogics {
  
  
  boolean serverLogicsDebugPopupFlag = false;
  
  QnactrSimulation sim;  
  
  public ServerLogics(QnactrSimulation Sim){
    sim = Sim;
  }
  
  /**
   * 
   * @param ServerName need to be lower case without space, because from auto valueOf
   * @param ServiceStage need to be lower case without space, because from auto valueOf
   * @param Entity
   * @return
   */
  public Object Enums(Enums.ServerName  ServerName, Enums.ServiceStage ServiceStage, Entity Entity) {
    String hmiID = Integer.toString(sim.ID);
    switch (ServerName) {
      
      case testbuffer:

        switch (ServiceStage){
          
          case Release:
            
            //            String texts = "Entity.Tag: " + Entity.Tag + ". Job ID: " + Entity.ownerJob.getId();
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n" + texts, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
            //            if(SimSystem.clock() < 3 ) sim.funs.createEntity("test buffer", "test buffer", "test buffer", "test1", 0.5);
            //            sim.funs.createEntity("test buffer", "test buffer", "test buffer", "test2", 1.0);
            //            sim.funs.createEntity("test buffer", "test buffer", "test buffer", "test3", 1.0);
            //            sim.funs.createEntity("test buffer", "test buffer", "test buffer", "test4", 1.0);
            
            return true;
            
          case Beginning:
            
            //            if(SimSystem.clock() < 1 ) sim.funs.createEntity("test buffer", "test buffer", "test buffer", "test1", 0.3);
                        
                        
            //            //test abort entity in server.
            //            if(SimSystem.clock() == 0.3){
            //              
            //              sim.funs.abortEntityInServer("testmodule", 0);
            //              
            //            } //end of test abort entity in server
                        
            
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
            break;
            
          case Ending:

            //            int testInt = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            //            String texts = "Entity.Tag: " + Entity.Tag + ". Entity.Entity_Type: " + Entity.Entity_Type + ". Entities # in test module: " + testInt + "\nQueueSize: " + sim.funs.getNumberOfQnactrEntityInQueue("testmodule");
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n" + texts, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            
                        
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
                        
            //            String createAtServerName = "test module";
            //            Entity newEntity = sim.funs.createEntity(createAtServerName, "test buffer", "test module", "test", 0.0);
            //            newEntity.Note = "create test note";
                        
            break;
            
          case Timing:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
            return 0.0;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;  
        }
        break;
        
        
      case testmodule:
        switch (ServiceStage){
          
          case Release:
           
            //            int testInt = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            //            String texts = "Entity.Tag: " + Entity.Tag + ". Entities # in test module: " + testInt + "\nRelease: " + Boolean.toString(testInt < 1) + "\nQueueSize: " + sim.funs.getNumberOfQnactrEntityInQueue(ServerName.toString());
            //            
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n" + texts, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            
            return  true; // (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < 1 );
            
            //            return  (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
            
          case Beginning:
            
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            break;
            
          case Ending:

            //            int testInt = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            //            String texts = "Entity.Tag: " + Entity.Tag + ". Entity.Entity_Type: " + Entity.Entity_Type + ". Entities # in test module: " + testInt + "\nQueueSize: " + sim.funs.getNumberOfQnactrEntityInQueue("testmodule");
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n" + texts, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            //            
                        
                        
                        
                        
                        
            //            //test abort entity in queue.
            //            if(SimSystem.clock() == 1.0){
            //              
            //              sim.funs.abortEntityInQueue(ServerName.toString(), 1);
            //              
            //            } //end test abort entity in queue
            
            
            
            
            
            break;
            
          case Timing:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            return 0.0;  
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break; 
        }
        break;
        
        
      case prescheduledevents:
        switch (ServiceStage){
          
          case Release:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            //(sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()))
            return true;
            
          case Beginning:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            //Scenario Events, cast delayed events to the delayed events server
            
            break;
            
          case Ending:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            //Scenario Events, process immediate events at Clock = 0
            
            
            //test
//            double testx1 = 11.4; 
//            double testx2 = 11.54;
//            double testx3 = 11.55;
//            double testx4 = 11.56;
//            double testx5 = 11.6;
//            System.out.println("test " + (int)testx1 + " "  + (int)testx2 + " " + (int)testx3 + " " + (int)testx4 + " " + (int)testx5 + " "); //results, get 11, the integer part
            
            //Event_00_Set_QN_Parameters
            //System.out.println("Clock " + SimSystem.clock() + ", Event_00_Set_QN_Parameters");
            
            /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            //data base
            sim.funs.MotorModuleFun__Initialize_Digram_Frequency_Table();
            
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            //model running related parameters
            
            //it seems uncessary
            //sim.vars.centralParametersModule.Stop_When_No_In_Model_Event_Pending = false;// true or false
            
            //end of model running related parameters
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            //function switches
            //sim.vars.audioModule.Listening_To_OnesOwn_Speech  = false; //true (by default) //used by ACT-R  Schumacher exp1 model, ToDo, set this  with sgp 
            //GlobalUtilities.popUpMessage("NOTICE! sim.vars.audioModule.Listening_To_OnesOwn_Speech  set to  false in Event_00_Set_QN_Parameters");
            
            
            //do not merge a buffer chunk into DM when the chunk is cleared from the Buffer, which can save a lot of computation resource when no retrieval request wants these chunks anyway.
            //ACT-R uses all true by default 
            
            /*
            sim.vars.declarativeModule.Merge_Aural_Buffer_Chunk = false;
            sim.vars.declarativeModule.Merge_Aural_Location_Buffer_Chunk = false;
            sim.vars.declarativeModule.Merge_Goal_Buffer_1_Chunk = false;
            sim.vars.declarativeModule.Merge_Goal_Buffer_2_Chunk = false;
            sim.vars.declarativeModule.Merge_Visual_Buffer_Chunk = false;
            sim.vars.declarativeModule.Merge_Visual_Location_Buffer_Chunk = false;

            GlobalUtilities.popUpMessage("NOTICE! declarativeModule.Merge less than optimal set in Event_00_Set_QN_Parameters");
            */
            
            sim.vars.intentionalModule.Multitasking_Schedule_Method = "threaded-cognition"; // = "threaded-cognition"; //or "" for ACT-R before threaded cognition (need :er t for random)  // = "goal-adaptation"; is not done
            //GlobalUtilities.popUpMessage("NOTICE! sim.vars.intentionalModule.Multitasking_Schedule_Method  set to  threaded-cognition in Event_00_Set_QN_Parameters");
            
            
            sim.vars.utilityModule.utility_Computation_Method = "temporal-difference-reinforcement"; //one of "temporal-difference-reinforcement", or "PG-C"
            //sim.vars.visionModule.Reencoding_Enabled = false; //true by default //for Altmann 2008, no reencoding makes it easier to control the model.
            //sim.vars.visionModule.Reencoding_Makes_Vision_Busy = false; // true by default
            //sim.vars.visionModule.Trigger_Procedural_Conflict_Resolution_After_Visual_Location_Buffer_Stuffing = false;  //true by default
            //end of function switches
            
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            
            //print, output, and debug control
            sim.vars.printingModule.Output_Trace = true ; //true by default   //traces in trace.txt
            sim.vars.printingModule.Output_Act_R_Noise_Random_Pick_Trace = true; //false by default 
            
            sim.vars.printingModule.Response_Results = true ; //true by default  //traces in response_results.txt
            sim.vars.printingModule.Output_Window_Trace = false; //false by default //traces in MicroSaint window
            
            //for multi run data collection mode:
            sim.vars.printingModule.Popout_Message = true; //true by default
            //sim.vars.printingModule.Clear_Output_Trace_Txt_Before_Each_Simulation = false;  //true by default, use false when collecting data from multiple MicroSaint runs
            //sim.vars.printingModule.Output_Trace_Append = true; //false by default, use false when collecting data from multiple MicroSaint runs
            //sim.vars.printingModule.Clear_Response_Results_Txt_Before_Each_Simulation   //true by default
            
            sim.vars.messageOn = false; //this is for debugging
            
            sim.vars.printingModule.Show_Multiple_Goal_Trace_In_Output_Trace_Txt = false; //false by default. 
            sim.vars.printingModule.Show_Display_And_Feedback_In_Output_Trace_Txt = true; //false by default
            //end of print, output, and debug control
            
            
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            //experiment Animator related
            if(QnactrSimulation.taskVisualization2DEnable){
              sim.vars.taskVisualization2D.hideObject(sim.vars.taskVisualization2D.visualAttentionCircleID); // Animator.HideComment("500");  //hide the visual attention red circle
              sim.vars.taskVisualization2D.hideObject(sim.vars.taskVisualization2D.mouseCursorID); // Animator.HideImage ("501");  //hide the mouse cursor
            }
            sim.vars.animatorModule.Show_Animator = true; //true by default.
            sim.vars.animatorModule.Show_Visual_Attention_Focus = true; //true by default.
            sim.vars.animatorModule.Play_Sound_Through_Speaker = true; //true by default.
            sim.vars.animatorModule.Vocalize_Speech = true; //true by default.
            
            //set default voice, can override default setting in the Sound_Plugin
            //Sound_Plugin.SelectVoiceForTextToSpeech("Microsoft Server Speech Text to Speech Voice (en-US, ZiraPro)");
            //Sound_Plugin.SelectVoiceForTextToSpeech("Microsoft Server Speech Text to Speech Voice (zh-CN, HuiHui)");
            
            //Network details Visualization related
            sim.vars.networkDetailsVisualizationModule.Show_Network_Details_Visualization = true;  //true by default.
            
            String [] all_animator3d_comment_ids = new String [] {
                "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                "21", "22", "23", "24" 
            }; 
            
            sim.vars.programGlobalVar__Animator3D_Comment_IDs = sim.funs.ProgramUtilitiesFun__StringArray_To_LinkedList_String (all_animator3d_comment_ids);
            
            sim.funs.AnimatorModuleFun__Animator3D_Show_Hide_All_Comments(false);
            
            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            
            //necessary global parameters
            sim.vars.centralParametersModule.Special_Chunk_Types_For_Programming.addLast ("_response-time_");
            sim.vars.centralParametersModule.Special_Chunk_Types_For_Programming.addLast ("_response-correctness_");
            sim.vars.intentionalModule.Multitasking_Goal_Buffer_Name_List = sim.funs.ProgramUtilitiesFun__StringArray_To_LinkedList_String(new String [] {"goal", "goal-2"} );
            
            //Initialize sim.vars.programGlobalVar__All_Server_Node_ID_List: a task ID list contains all the server IDs. 
            //add more IDs if more modules and buffers are added
            
            //these were used in C# version, may not be needed in Java version
            /*
            String [] all_server_node_id_array = new String [] {
                "100", "101", "221",  
                "241", "242",
                "203", "222", 
                "201", "203", "103", "10", "11", "12", "6", "7",  
                "20", "22", "23", "24", "21",
                "110", "111", "112", "113",
                "5", "3", "30", "32", "33", "34", "31",
                "105", "13", "14", "15", "1", "2", "4", "8", "9"
            }; 
            sim.vars.programGlobalVar__All_Server_Node_ID_List = sim.funs.ProgramUtilitiesFun__StringArray_To_LinkedList_String (all_server_node_id_array);
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("first trigger entity", "100");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Entity Direct Cast Delay", "101");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Control Motor", "221");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Control To Display", "241");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Dynamic Events", "242");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Control Voice Key", "222");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Visual and Audio Display Schedule", "201");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Timer Triggering Feedback Or Display", "203");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Visual Display", "103");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Vision Module", "10");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Visual Buffer", "11");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Visual-location Buffer", "12");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Declarative Module", "6");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Retrieval Buffer", "7");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Motor Module", "20");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Motor Preparation", "22");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Motor Initiation", "23");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Motor Execution", "24");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Manual Buffer", "21");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Trigger Buffer", "110");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Trigger Buffer Loop", "111");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Vision Module Trigger", "112");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Vision Module Trigger Loop", "113");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Matching And Selection", "5");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Execution", "3");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Speech Module", "30");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Speech Preparation", "32");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Speech Initiation", "33");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Speech Execution", "34");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Vocal Buffer", "31");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Audio Display", "105");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Audio Module", "13");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Aural Buffer", "14");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Aural-location Buffer", "15");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Intentional Module", "1");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Goal Buffer", "2");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Goal Buffer-2", "4");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Imaginary Module", "8");
            sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.Add("Imaginal Buffer", "9");
            */
            
            //high-level cognition process module (buffer) name list, which are used to identify buffer name in a rule's action part.
            //sim.vars.programGlobalVar__High_Level_Process_Module_Name_List.Add("retrieval");
            //sim.vars.programGlobalVar__High_Level_Process_Module_Name_List.Add("imaginal"); //ACT-R allow imaginal and retrieval actions in the same rule, e.g., retireve-associate in paired-learning, so here consider imaginal as a low level action.
            
            /*
            sim.vars.centralParametersModule.Use_Procedural_Resources = true;  //false by default, exploring ideas for QN features
            GlobalUtilities.popUpMessage("NOTICE! sim.vars.centralParametersModule.Use_Procedural_Resources  set to  true in Event_00_Set_QN_Parameters");
             */
            
            if (sim.vars.centralParametersModule.Use_Procedural_Resources) {
              sim.vars.productionModule.Processor_Low_Level_Remaining_Capacity = sim.vars.productionModule.Processor_Low_Level_Max_Capacity;
              sim.vars.productionModule.Processor_High_Level_Remaining_Capacity = sim.vars.productionModule.Processor_High_Level_Max_Capacity;
              sim.vars.productionModule.Processor_Aural_Action_Remaining_Number = sim.vars.productionModule.Processor_Aural_Action_Max_Number;
              sim.vars.productionModule.Processor_Aural_Location_Action_Remaining_Number = sim.vars.productionModule.Processor_Aural_Location_Action_Max_Number;
              sim.vars.productionModule.Processor_Goal_Action_Remaining_Number = sim.vars.productionModule.Processor_Goal_Action_Max_Number;
              sim.vars.productionModule.Processor_Imaginal_Action_Remaining_Number = sim.vars.productionModule.Processor_Imaginal_Action_Max_Number;
              sim.vars.productionModule.Processor_Manual_Action_Remaining_Number = sim.vars.productionModule.Processor_Manual_Action_Max_Number;
              sim.vars.productionModule.Processor_Retrieval_Action_Remaining_Number = sim.vars.productionModule.Processor_Retrieval_Action_Max_Number;
              sim.vars.productionModule.Processor_Visual_Action_Remaining_Number = sim.vars.productionModule.Processor_Visual_Action_Max_Number;
              sim.vars.productionModule.Processor_Visual_Location_Action_Remaining_Number = sim.vars.productionModule.Processor_Visual_Location_Action_Max_Number;
              sim.vars.productionModule.Processor_Vocal_Action_Remaining_Number = sim.vars.productionModule.Processor_Vocal_Action_Max_Number;
              
            }
            else {
              sim.vars.productionModule.Processor_Low_Level_Max_Capacity = 0;
              sim.vars.productionModule.Processor_High_Level_Max_Capacity = 0;
            }
            
            sim.vars.programGlobalVar__Event_Priority_Table.put("Vision Module Trigger Loop", 1 );
            sim.vars.programGlobalVar__Event_Priority_Table.put("Trigger Buffer Loop", 10 );
            //end of necessary global parameters

            ///////////
            /// ////// task specific parameter setting
            /// /////
            
            /*
            //shooting 
            */
            //MotorModuleFun__Add_Move_Cursor_Noise
            //GlobalUtilities.popUpMessage("NOTICE! sim.funs.MotorModuleFun__Add_Move_Cursor_Noise target_width_in_approaching_direction_in_pixel changed");
                        
                        
            
            //Event_01_Clear_Output_Txt_Files
            //System.out.println("Clock " + SimSystem.clock() + ", Event_01_Clear_Output_Txt_Files");
            
            //C# way
            /*
            if(sim.vars.printingModule.Clear_Output_Trace_Txt_Before_Each_Simulation && (Model.RunNumber == 1) ){
              int i=1;
              int empty_lines = 0; //track continuously empty lines
              while ( true ){
                String temp_line = Communication.Trace.GetLine(i);
                if(temp_line.equals( "" )){
                  empty_lines++;
                }
                else { //reset 
                  empty_lines = 0;
                }
                if(empty_lines < 100){ // Trace output should not have more than 99 lines empty continously
                  Communication.Trace.SetLine ("", i);
                }
                else break;
                i++;
              }
            } 
            
            if( sim.vars.printingModule.Output_Trace_Append == true) {
              sim.vars.centralParametersModule.Trace_Line_Number = sim.funs.ProgramUtilitiesFun__Get_Number_Of_Lines_In_Trace_Txt_File () + 1;
              //System.out.println(sim.vars.centralParametersModule.Trace_Line_Number);
            }
            */
            
            //QN-Java
            if(sim.vars.printingModule.Clear_Output_Trace_Txt_Before_Each_Simulation ){ // TODO && (Model.RunNumber == 1) 
               sim.files.Results_trace.clear();
            }
            else {
              //do nothing
            }
            
            if( sim.vars.printingModule.Output_Trace_Append) {
              //do nothing, this is now by default, if not using Clear_Output_Trace_Txt_Before_Each_Simulation
            }
              
            //DONE. test here with Output tract
            //sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("The_Output test in Server Logics.java");
            
            //C# way
            /*
            if(sim.vars.printingModule.Clear_Response_Results_Txt_Before_Each_Simulation && (Model.RunNumber == 1) ){
              int i=1;
              int empty_lines = 0; //track continuously empty lines
              while ( true ){
                String temp_line = Communication.Response_Results.GetLine(i);
                if(temp_line.equals( "" )){
                  empty_lines++;
                }
                else { //reset 
                  empty_lines = 0;
                }
                if(empty_lines < 100){ // Response_Results should not have more than 99 lines empty continously
                  Communication.Response_Results.SetLine ("", i);
                }
                else break;
                i++;
              }
            } 

            if(sim.vars.printingModule.Clear_Human_Drive_Results_Txt_Before_Each_Simulation && (Model.RunNumber == 1) ){
              int i=1;
              int empty_lines = 0; //track continuously empty lines
              while ( true ){
                String temp_line = Communication.Human_Drive_Results.GetLine(i);
                if(temp_line.equals( "" )){
                  empty_lines++;
                }
                else { //reset 
                  empty_lines = 0;
                }
                if(empty_lines < 100){ // Human_Drive_Results should not have more than 99 lines empty continously
                  Communication.Human_Drive_Results.SetLine ("", i);
                }
                else break;
                i++;
              }
            } 



            if(sim.vars.printingModule.Clear_Eye_Movement_Results_Txt_Before_Each_Simulation && (Model.RunNumber == 1) ){
              int i=1;
              int empty_lines = 0; //track continuously empty lines
              while ( true ){
                String temp_line = Communication.Eye_Movement_Results.GetLine(i);
                if(temp_line.equals( "" )){
                  empty_lines++;
                }
                else { //reset 
                  empty_lines = 0;
                }
                if(empty_lines < 100){ // Eye_Movement_Results should not have more than 99 lines empty continously
                  Communication.Eye_Movement_Results.SetLine ("", i);
                }
                else break;
                i++;
              }
            } 




            if(sim.vars.printingModule.Clear_Foot_Results_Txt_Before_Each_Simulation && (Model.RunNumber == 1) ){
              int i=1;
              int empty_lines = 0; //track continuously empty lines
              while ( true ){
                String temp_line = Communication.Foot_Results.GetLine(i);
                if(temp_line.equals( "" )){
                  empty_lines++;
                }
                else { //reset 
                  empty_lines = 0;
                }
                if(empty_lines < 100){ // Foot_Results should not have more than 99 lines empty continously
                  Communication.Foot_Results.SetLine ("", i);
                }
                else break;
                i++;
              }
            } 
            
            */
            
            if(sim.vars.printingModule.Clear_Response_Results_Txt_Before_Each_Simulation  ){//TODO  && (Model.RunNumber == 1)
              sim.files.Results_results_response.clear();
            } 

            if(sim.vars.printingModule.Clear_Human_Drive_Results_Txt_Before_Each_Simulation ){//TODO  && (Model.RunNumber == 1)
              sim.files.Results_results_human_drive.clear();
            } 

            if(sim.vars.printingModule.Clear_Eye_Movement_Results_Txt_Before_Each_Simulation  ){ //TODO  && (Model.RunNumber == 1)
              sim.files.Results_results_eye_movement.clear();
            } 

            if(sim.vars.printingModule.Clear_Foot_Results_Txt_Before_Each_Simulation ){ //TODO  && (Model.RunNumber == 1)
              sim.files.Results_results_foot.clear();
            } 
            
            if(sim.vars.printingModule.Clear_Mental_Workload_Results_Txt_Before_Each_Simulation ){ //TODO  && (Model.RunNumber == 1)
              sim.files.Results_mental_workload.clear();
            } 
            sim.files.Results_mental_workload.writeLine("ClockTime(s)_UtilizationValuesFrom:" + "\t" + "Vision_Module" + "\t" + "Audio_Module" + "\t" + "Perceptual_SubNetwork" + "\t" + "Production_Module" + "\t" + "Declarative_Module" + "\t" + "Imaginary_Module" + "\t" + "Cognitive_SubNetwork" + "\t" + "Motor_Module" + "\t" + "Speech_Module" + "\t" + "Motor_SubNetwork" + "\t" + "Overall_Utilization");
            
            
            
            
            //Event_02_Set_ACTR_Defaults
            //System.out.println("Clock " + SimSystem.clock() + ", Event_02_Set_ACTR_Defaults");
            
            //1. load compilation excel sheet. prepare production compilation module.
            sim.funs.ProductionCompilationModuleFun__Load_Compilation_Criteria();

            
            //2. define forbidden names in production rules. these names will not be automatically defined as default chunk
            String [] temp_array = new String[] {"nil", "error", "busy", "free", "empty", "full", "lowest", "highest", "current" };
            sim.vars.productionModule.Forbidden_Name_In_Production_Rule = sim.funs.ProgramUtilitiesFun__StringArray_To_LinkedList_String(temp_array);

            //3. load default chunk-types, chunks
            LinkedList<String> lines_without_comments = sim.funs.ParametersFun__Load_ACTR_Default_Into_Lines();
            LinkedList<String> standard_lists = sim.funs.ParametersFun__Standardize_ACTR_Model_Lines( lines_without_comments );
            LinkedList<String> a_list = sim.funs.ParametersFun__Remove_A_List_From_Initialization_Lists (standard_lists);
            while(a_list != null){
              String function_name = (String) a_list.getFirst();
              switch(function_name){
                case "chunk-type":  {
                  a_list.removeFirst(); //remove chunk-type
                  sim.funs.ChunkFun__Add_Chunk_Type(a_list);
                break;
                }
                case "chunk-type-include":  { //ACT-R have this function as :include parameter of function chunk-type
                  a_list.removeFirst(); //remove chunk-type-include
                  sim.funs.ChunkFun__Add_Chunk_Include_Type(a_list);
                break;
                }
                case "define-chunks": { //add chunks to model chunk list.
                  a_list.removeFirst(); //remove "define-chunks"
                  LinkedList<String> temp_list = sim.funs.ParametersFun__Remove_A_List_From_Initialization_Lists (a_list);
                  while(temp_list != null){
                    Chunk temp_chunk = sim.funs.ChunkFun__Make_Chunk_From_Descritption(sim.funs.ProgramUtilitiesFun__LinkedListString_To_ArrayString(temp_list));
                    if (sim.funs.ChunkFun__Is_Chunk_Name(temp_chunk.Chunk_Name) == false)  sim.funs.ChunkFun__Define_Chunk(temp_chunk); //define this chunk in model Chunks list if it is not defined
                    temp_list = sim.funs.ParametersFun__Remove_A_List_From_Initialization_Lists (a_list);
                  }
                break;
                }
                default:  {
                  System.out.println ("Event_02_Set_ACTR_Defaults error! the function_name is undefined: " + function_name);
                break;
                }
              }
              a_list = sim.funs.ParametersFun__Remove_A_List_From_Initialization_Lists (standard_lists);
            }

            //4. initialize information used in motor module.
            sim.funs.DeviceModuleFun__Initialize_Device_Interface_Hashtables();

            sim.vars.motorModule.Right_Hand.Finger_Location_Offsets.put("index",  sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(0, 0)  );
            sim.vars.motorModule.Right_Hand.Finger_Location_Offsets.put("middle", sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(1, 0)  );
            sim.vars.motorModule.Right_Hand.Finger_Location_Offsets.put("ring",   sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(2, 0)  );
            sim.vars.motorModule.Right_Hand.Finger_Location_Offsets.put("pinkie", sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(3, 0)  );
            sim.vars.motorModule.Right_Hand.Finger_Location_Offsets.put("thumb",  sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(-1, 2)  );

            sim.vars.motorModule.Left_Hand.Finger_Location_Offsets.put("index",  sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(0, 0)  );
            sim.vars.motorModule.Left_Hand.Finger_Location_Offsets.put("middle", sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(-1, 0)  );
            sim.vars.motorModule.Left_Hand.Finger_Location_Offsets.put("ring",   sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(-2, 0)  );
            sim.vars.motorModule.Left_Hand.Finger_Location_Offsets.put("pinkie", sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(-3, 0)  );
            sim.vars.motorModule.Left_Hand.Finger_Location_Offsets.put("thumb",  sim.funs.ProgramUtilitiesFun__Make_Two_Tuple(1, 2)  );

            sim.funs.MotorModuleFun__Home_Hands(); //by default, position hands at keyboard default location

            //Event_03_Load_QN_ACTR_Model_Initialization
            //System.out.println("Clock " + SimSystem.clock() + ", Event_03_Load_QN_ACTR_Model_Initialization");
            
            sim.funs.ParametersFun__Load_QN_ACTR_Model_Initialization();

	         //after setting the :seed parameter, determine whether to randomize the seed for display item randomness.
	         if( sim.vars.centralParametersModule.Randomize_Seed_For_Display_Sequence ) {
	        	 sim.vars.programGlobalVar__Rand1_Seed1 = GlobalUtilities.randomInteger(1, 2147483562); // [1, 2147483562]
	            sim.vars.programGlobalVar__Rand1_Seed2 = GlobalUtilities.randomInteger(1, 2147483398); // [1, 2147483398]
	            //System.out.println("Rand1_Seed1: " + sim.vars.programGlobalVar__Rand1_Seed1 + " Rand1_Seed2: " + sim.vars.programGlobalVar__Rand1_Seed2);
	         }


	         //Event_04a_Initialize_Trial_Display_Response_List_From_Template
	         //System.out.println("Clock " + SimSystem.clock() + ", Event_04a_Initialize_Trial_Display_Response_List_From_Template");
	          
		     //old //aa_Altmann_Study1_Initialize_Trial_List();
	
	         sim.funs.TaskTemplateFun__Initialize_Experiment_Trial_List_From_Template();

	          /*
	          //here different methods can be used to initialize the trials of the task
	          //if single discrete task template is used
	          if(sim.vars.taskTemplate.Method.equals( "single_discrete_task_visual_display"))TaskTemplateFun__Obsolete_Initialize_Single_Discrete_Task_Visual_Display_Template_Trial_List();
	          */
	
	          /*
	          //if keyboard_single_key_per_trial is used
	          if(sim.vars.taskTemplate.Response.equals( "keyboard_single_key_per_trial"))
	          {}
	          */
	
	          //write/output the initialized experiment trial parameter list to external file: Experiment_Trial_Parameter_List.txt
	         if(sim.vars.taskTemplate.Output_Initialized_Experiment_Trial_Parameter_List_To_Experiment_Trial_Parameter_List_Txt.equals( "yes")){
	            sim.funs.ProgramUtilitiesFun__Clear_Experiment_Trial_Parameter_List();
	            sim.funs.ProgramUtilitiesFun__Output_Initialized_Experiment_Trial_Parameter_List_To_Experiment_Trial_Parameter_List_Txt();
	          }

          
          
	          //Event_04b_Load_Customized_Trial_Display_Response_List_From_File
	          //System.out.println("Clock " + SimSystem.clock() + ", Event_04b_Load_Customized_Trial_Display_Response_List_From_File");
	          
	        //Event_04b is only meaningful when the external trial parameter file is not made by Event_04a but by users; however, it does no harm if the external file is made by Event_04a using templates. It just wastes time.
	
	        //load external file: Experiment_Trial_Parameter_List.txt as the experiment trial parameter list in sim.vars.centralParametersModule.Experiment_Trial_Parameter_List
	          //ToDo
	
	
	          //also need to: From sim.vars.centralParametersModule.Experiment_Trial_Parameter_List, make sim.vars.centralParametersModule.Experiment_Trial_Display_Item_List, which will be used in display codes.
	          
	
	
	          //Event_05_World3D_Initialization
	          //System.out.println("Clock " + SimSystem.clock() + ", Event_05_World3D_Initialization");
	          
	          Hashtable additional_objects = new Hashtable();
	          //first round
	          
	          Iterator<Entry<String, Object>>itr_Objects = sim.vars.world3DTemplate.World.Objects.entrySet().iterator();
	          while (itr_Objects.hasNext()){
	        	  Entry<String, Object> an_object = itr_Objects.next();
	        	  if(an_object.getValue() instanceof World3D_DriverCar){
		            //get the start-on road ID
		            World3D_DriverCar driver_car = (World3D_DriverCar)an_object.getValue();
		            String start_on_road_name = driver_car.Start_Road_Name;
		            World3D_Road the_start_road = new World3D_Road();
		            boolean road_found = false;
		            
		            Iterator<Entry<String, Object>>itr_Objects2 = sim.vars.world3DTemplate.World.Objects.entrySet().iterator();
		            while (itr_Objects2.hasNext()){
		              //            foreach(DictionaryEntry an_object2 in  sim.vars.world3DTemplate.World.Objects){
		              Entry<String, Object> an_object2 = itr_Objects2.next();
		              if( (an_object2.getValue() instanceof World3D_Road) && ((World3D_Road)an_object2.getValue()).Name.equals( start_on_road_name)){
		                road_found = true;
		                the_start_road = (World3D_Road)an_object2.getValue();
		              }
		            }
		            if(!road_found){
		              System.out.println("Error! Event_05_World3D_Initialization cannot find road with name: " + start_on_road_name + " for driver_car.");
		              SimSystem.abort();
		            }
		            driver_car.Start_Road_World3D_ID = the_start_road.World3D_ID;
            
            
		            World3D_Template_Driving_Method the_method = null;
		            if(sim.vars.world3DTemplate.Method_Object != null && sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) the_method = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
		        
		            //#region sp-driv   
		            if(the_method != null && (the_method.Who_Drive.toLowerCase().equals( "model-6dof-speed") || the_method.Who_Drive.toLowerCase().equals( "human-6dof-speed" )) ){ 
		              driver_car.Vehicle_Basic.On_Road_World3D_ID = driver_car.Start_Road_World3D_ID;
		              driver_car.Vehicle_Basic.Loc_X = (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob1;
		              driver_car.Vehicle_Basic.Loc_Y = (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob2;
		              driver_car.Vehicle_Basic.Loc_Z = (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob3;
		              driver_car.Vehicle_Basic.Pitch = (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob4;
		              driver_car.Vehicle_Basic.Yaw =  (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob5;
		              driver_car.Vehicle_Basic.Roll =   (double)sim.vars.spaceDrivingVar__Self_Initiatial_Location_And_Rotation.Ob6;
		            }
		            //#endregion    
		            else{ //normal driving
		              sim.funs.TaskTemplateFun__World3D_Reset_DriverCar_To_Start_Position(driver_car);
		            }
		            
		            /*
		            if(driver_car.Start_Distance>= 0.0){  // key for normal start
		              Three_Tuple global_parameters = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(driver_car.Start_Road_World3D_ID, driver_car.Start_Distance, driver_car.Start_Lane_Num * the_start_road.Lane_Width, 0.0   ); //start on the center of a lane, and with heading angle same as the road.
		              
		              //initializae vehicle basic
		              driver_car.Vehicle_Basic.Angle_Between_Road_Direction = 0.0;
		              driver_car.Vehicle_Basic.Distance_From_Start = driver_car.Start_Distance;
		              driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center = driver_car.Start_Lane_Num * the_start_road.Lane_Width;
		              driver_car.Vehicle_Basic.Lane_Num = driver_car.Start_Lane_Num;
		              
		              
		              driver_car.Vehicle_Basic.Loc_X = (double)global_parameters.Ob1;
		              driver_car.Vehicle_Basic.Loc_Y = 0.0; //current does not support altitude
		              driver_car.Vehicle_Basic.Loc_Z = (double)global_parameters.Ob2;
		              driver_car.Vehicle_Basic.Pitch = 0.0;
		              driver_car.Vehicle_Basic.Roll = 0.0;
		              driver_car.Vehicle_Basic.Yaw = (double)global_parameters.Ob3;
		              
		              
		              
		            }
		            else{ // driver_car.Start_Distance < 0 is the key for global initialization, 6DOF use customized data
		              Three_Tuple local_info = sim.funs.ProgramUtilitiesFun__World3D_Global_Loc_And_Angle_To_Local( driver_car.Vehicle_Basic.On_Road_World3D_ID, driver_car.Vehicle_Basic.Loc_X, driver_car.Vehicle_Basic.Loc_Z  , driver_car.Vehicle_Basic.Yaw, 0.0);
		              driver_car.Start_Distance = (double)local_info.Ob1;
		              driver_car.Vehicle_Basic.Distance_From_Start = (double)local_info.Ob1;
		              driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center = (double)local_info.Ob2;
		              driver_car.Vehicle_Basic.Angle_Between_Road_Direction = (double)local_info.Ob3;
		            }
		            */
	        	  } //end of World3D_DriverCar
	        	  else if(an_object.getValue() instanceof World3D_OtherCar){
		            //get the start-on road ID
		            World3D_OtherCar other_car = (World3D_OtherCar)an_object.getValue();
		            String start_on_road_name = other_car.Start_Road_Name;
		            World3D_Road the_start_road = new World3D_Road();
		            boolean road_found = false;
		            
		            Iterator<Entry<String, Object>>itr_Objects2 = sim.vars.world3DTemplate.World.Objects.entrySet().iterator();
		            while (itr_Objects2.hasNext()){
		              Entry<String, Object> an_object2 = itr_Objects2.next();
		              if( (an_object2.getValue() instanceof World3D_Road) && ((World3D_Road)an_object2.getValue()).Name.equals( start_on_road_name)){
		                road_found = true;
		                the_start_road = (World3D_Road)an_object2.getValue();
		              }
		            }
		            if(!road_found){
		              System.out.println("Error! Event_05_World3D_Initialization cannot find road with name: " + start_on_road_name + " for driver_car.");
		              SimSystem.abort();
		            }
		            
		            other_car.Start_Road_World3D_ID = the_start_road.World3D_ID;
		            
		            sim.funs.TaskTemplateFun__World3D_Reset_OtherCar_To_Start_Position(other_car);		            
		          } //end of World3D_OtherCar
		          else if (an_object.getValue() instanceof World3D_Road){
		            //nothing, waiting for driver car to get its on road world3d ID
		          } //end of World3D_Road
		          else{
		            System.out.println("Error! Event_05_World3D_Initialization has undefined world3d object type");  
		            SimSystem.abort();
		          }
	          } //end of first round

	          //second round
          
	          itr_Objects = sim.vars.world3DTemplate.World.Objects.entrySet().iterator();
	          while (itr_Objects.hasNext()){
	        	  Entry<String, Object> an_object = itr_Objects.next();

	        	  if (an_object.getValue() instanceof World3D_Road){ //identify driver-on road, and add far points objects
		              //add far point object for the driving start on road
		              World3D_Road a_road = (World3D_Road)an_object.getValue();
		              World3D_DriverCar the_driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
		              if(a_road.Name.equals( the_driver_car.Start_Road_Name)){
		                //create three far point object for the center lane
		                //lane 0, center
		                World3D_Point a_point = new World3D_Point();
		                a_point.Distance_From_Start = the_driver_car.Start_Distance;
		                a_point.Distance_Lateral_To_Center_Lane_Center = 0.0;
		                a_point.On_Road_Lane = 0;
		                a_point.On_Road_World3D_ID = a_road.World3D_ID;
		                a_point.Type = "far-point-lane-center";
		                
		                String world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
		                sim.vars.world3DTemplate.World.Object_Num ++;
		                a_point.World3D_ID = world3d_id;
		                additional_objects.put(world3d_id, a_point.clone() );
		                
		                
			             //lane 0, left
			             a_point = new World3D_Point();
			             a_point.Distance_From_Start = the_driver_car.Start_Distance;
			             a_point.Distance_Lateral_To_Center_Lane_Center = 0.0 - a_road.Lane_Width / 2.0;
			             a_point.On_Road_Lane = 0;
			             a_point.On_Road_World3D_ID = a_road.World3D_ID;
			             a_point.Type = "far-point-lane-left";
			              
			             world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			             sim.vars.world3DTemplate.World.Object_Num ++;
			             a_point.World3D_ID = world3d_id;
			             additional_objects.put(world3d_id, a_point.clone() );
			                
			             //lane 0, right
			             a_point = new World3D_Point();
			             a_point.Distance_From_Start = the_driver_car.Start_Distance;
			             a_point.Distance_Lateral_To_Center_Lane_Center = 0.0 + a_road.Lane_Width / 2.0;
			             a_point.On_Road_Lane = 0;
			             a_point.On_Road_World3D_ID = a_road.World3D_ID;
			             a_point.Type = "far-point-lane-right";
			              
			             world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			             sim.vars.world3DTemplate.World.Object_Num ++;
			             a_point.World3D_ID = world3d_id;
			             additional_objects.put(world3d_id, a_point.clone() );
			              
			             int i;
			             for (i = 1; i <= a_road.Lane_Num_Left; i ++){ //add far point lane left for the left lanes if there is any
			               //lane -1 * i, center
			               a_point = new World3D_Point();
			               a_point.Distance_From_Start = the_driver_car.Start_Distance;
			               a_point.Distance_Lateral_To_Center_Lane_Center = -1 * i * a_road.Lane_Width ;
			               a_point.On_Road_Lane = -1 * i;
			               a_point.On_Road_World3D_ID = a_road.World3D_ID;
			               a_point.Type = "far-point-lane-center";
			                
			               world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			               sim.vars.world3DTemplate.World.Object_Num ++;
			               a_point.World3D_ID = world3d_id;
			               additional_objects.put(world3d_id, a_point.clone() );
			                
			               //lane -1 * i, left
			               a_point = new World3D_Point();
			               a_point.Distance_From_Start = the_driver_car.Start_Distance;
			               a_point.Distance_Lateral_To_Center_Lane_Center = -1 * i * a_road.Lane_Width - a_road.Lane_Width / 2.0;
			               a_point.On_Road_Lane = -1 * i;
			               a_point.On_Road_World3D_ID = a_road.World3D_ID;
			               a_point.Type = "far-point-lane-left";
			                
			               world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			               sim.vars.world3DTemplate.World.Object_Num ++;
			               a_point.World3D_ID = world3d_id;
			               additional_objects.put(world3d_id, a_point.clone() );
			              }
			              
			              for (i = 1; i <= a_road.Lane_Num_Right; i ++){ //add far point lane right for the right lanes if there is any
			                //lane 1 * i, center
			                a_point = new World3D_Point();
			                a_point.Distance_From_Start = the_driver_car.Start_Distance;
			                a_point.Distance_Lateral_To_Center_Lane_Center = 1 * i * a_road.Lane_Width ;
			                a_point.On_Road_Lane = 1 * i;
			                a_point.On_Road_World3D_ID = a_road.World3D_ID;
			                a_point.Type = "far-point-lane-center";
			                
			                world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			                sim.vars.world3DTemplate.World.Object_Num ++;
			                a_point.World3D_ID = world3d_id;
			                additional_objects.put(world3d_id, a_point.clone() );
			                
			                
			                //lane i, right
			                a_point = new World3D_Point();
			                a_point.Distance_From_Start = the_driver_car.Start_Distance;
			                a_point.Distance_Lateral_To_Center_Lane_Center = 1 * i * a_road.Lane_Width + a_road.Lane_Width / 2.0;
			                a_point.On_Road_Lane = 1 * i;
			                a_point.On_Road_World3D_ID = a_road.World3D_ID;
			                a_point.Type = "far-point-lane-right";
			                
			                world3d_id = Integer.toString(sim.vars.world3DTemplate.World.Object_Num); //start from 0
			                sim.vars.world3DTemplate.World.Object_Num ++;
			                a_point.World3D_ID = world3d_id;
			                additional_objects.put(world3d_id, a_point.clone() );
			              }
		              }
		          } //end of World3D_Road
	          } //end of second round
		      //add any additional objects
		      sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value_From_Another_Hashtable( sim.vars.world3DTemplate.World.Objects, additional_objects);

		        //#region Customized World3D objects
		
		        /*  
		          #region Space 1D speed control driving demo
		          
		          //add target point
		          World3D_Point a_point_2 = new World3D_Point();
		          World3D_DriverCar the_driver_car_pointer = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
		          a_point_2.Type = "customized-local-point";
		          a_point_2.Name = "End_Point"; // to mark the point for Production Rule knowledge.
		          //World3D_DriverCar the_driver_car = (TaskTemplateFun__Get_World3D_DriverCar_Object()).Vehicle_Basic.On_Road_World3D_ID;
		          //the_driver_car.Vehicle_Basic.On_Road_World3D_ID;
		          a_point_2.On_Road_World3D_ID = the_driver_car_pointer.Vehicle_Basic.On_Road_World3D_ID;
		
		          a_point_2.Distance_From_Start = 1000.0;
		          a_point_2.Distance_Lateral_To_Center_Lane_Center = 0.0;
		          a_point_2.Color = Color.LightYellow;
		          a_point_2.Size_X = 10.0; // diameter in meters
		          a_point_2.Size_Y = 10.0; 
		          a_point_2.Size_Z = 10.0; 
		
		          a_point_2.World3D_ID = sim.vars.world3DTemplate.World.Object_Num.ToString(); //start from 0
		          sim.vars.world3DTemplate.World.Object_Num ++;
		          sim.vars.world3DTemplate.World.Objects.put(a_point_2.World3D_ID, a_point_2  );
		          
		          //add ruler lines
		          World3D_Line_Segment a_line_seg = new World3D_Line_Segment();
		          double ruler_line_distance_limit = 1500.0;
		          
		          double distance ;
		          //for short lines
		          for ( distance = 50.0; distance < ruler_line_distance_limit ; distance += 100.0 ){
		            a_line_seg.Type = "customized-local-line-segment";
		            a_line_seg.On_Road_World3D_ID = the_driver_car_pointer.Vehicle_Basic.On_Road_World3D_ID;
		            a_line_seg.P1_Distance_From_Start = distance;
		            a_line_seg.P2_Distance_From_Start = distance; 
		            a_line_seg.P1_Distance_Lateral_To_Center_Lane_Center = -25.0;
		            a_line_seg.P2_Distance_Lateral_To_Center_Lane_Center =  25.0;
		            a_line_seg.Color = Color.LightGray;
		            
		            Three_Tuple p1_global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(a_line_seg.On_Road_World3D_ID, a_line_seg.P1_Distance_From_Start, a_line_seg.P1_Distance_Lateral_To_Center_Lane_Center, 0.0);
		            a_line_seg.P1_Loc_X = (double)p1_global_para.Ob1;
		            a_line_seg.P1_Loc_Z = (double)p1_global_para.Ob2;
		            
		            Three_Tuple p2_global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(a_line_seg.On_Road_World3D_ID, a_line_seg.P2_Distance_From_Start, a_line_seg.P2_Distance_Lateral_To_Center_Lane_Center, 0.0);
		            a_line_seg.P2_Loc_X = (double)p2_global_para.Ob1;
		            a_line_seg.P2_Loc_Z = (double)p2_global_para.Ob2;
		            
		            a_line_seg.World3D_ID = sim.vars.world3DTemplate.World.Object_Num.ToString(); //start from 0
		            sim.vars.world3DTemplate.World.Object_Num ++;
		            sim.vars.world3DTemplate.World.Objects.put(a_line_seg.World3D_ID, a_line_seg.Clone()  );
		          }
		          
		          
		          //for long lines
		          for ( distance = 0.0; distance < ruler_line_distance_limit ; distance += 100.0 ){
		            a_line_seg.Type = "customized-local-line-segment";
		            a_line_seg.On_Road_World3D_ID = the_driver_car_pointer.Vehicle_Basic.On_Road_World3D_ID;
		            a_line_seg.P1_Distance_From_Start = distance;
		            a_line_seg.P2_Distance_From_Start = distance; 
		            a_line_seg.P1_Distance_Lateral_To_Center_Lane_Center = -100.0;
		            a_line_seg.P2_Distance_Lateral_To_Center_Lane_Center =  100.0;
		            a_line_seg.Color = Color.Gray;
		            
		            Three_Tuple p1_global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(a_line_seg.On_Road_World3D_ID, a_line_seg.P1_Distance_From_Start, a_line_seg.P1_Distance_Lateral_To_Center_Lane_Center, 0.0);
		            a_line_seg.P1_Loc_X = (double)p1_global_para.Ob1;
		            a_line_seg.P1_Loc_Z = (double)p1_global_para.Ob2;
		            
		            Three_Tuple p2_global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(a_line_seg.On_Road_World3D_ID, a_line_seg.P2_Distance_From_Start, a_line_seg.P2_Distance_Lateral_To_Center_Lane_Center, 0.0);
		            a_line_seg.P2_Loc_X = (double)p2_global_para.Ob1;
		            a_line_seg.P2_Loc_Z = (double)p2_global_para.Ob2;
		            
		            a_line_seg.World3D_ID = sim.vars.world3DTemplate.World.Object_Num.ToString(); //start from 0
		            sim.vars.world3DTemplate.World.Object_Num ++;
		            sim.vars.world3DTemplate.World.Objects.put(a_line_seg.World3D_ID, a_line_seg.Clone()  );
		          }
		
		          #endregion
		        */
		        //#endregion
		      if( sim.vars.world3DTemplate.Method_Object != null ){
		        if (sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) {
		          sim.funs.VisionModuleFun__Update_World3D_Driving_Method_Visible_Objects();
		        }
		      }
		      //System.out.println("Event_05_World3D_Initialization DONE");
		      
		        //Event_06_Animator_Initialization
		        //System.out.println("Clock " + SimSystem.clock() + ", Event_06_Animator_Initialization");
		        
		      //World3D_DriverCar driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
		      //System.out.println("Event_06_Animator_Initialization " +  driver_car.Vehicle_Basic.Distance_From_Start);
		
		      //Animator initialization
		      if(sim.vars.animatorModule.Show_Animator){  //display hand default resting places
		    	  String[] hands = new String[] {"left", "right" };
	
		    	  ArrayList<String> hands_list = sim.funs.ProgramUtilitiesFun__StringArray_To_ArrayListString (hands);
	
		    	  String[] fingers = new String[] {"thumb", "index" , "middle", "ring", "pinkie" };
		    	  List<String> fingers_list = sim.funs.ProgramUtilitiesFun__StringArray_To_ArrayListString (fingers);
	
		    	  for( String hand : hands_list){
		    		  for (String finger : fingers_list){
		    			  if(hand.equals("left") && sim.vars.motorModule.Left_Hand.Object_Type_In_Hand.equals("touchscreen")) continue;
		    			  if(hand.equals("right") && sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals("touchscreen")) continue;
	
		    			  Two_Tuple finger_resting_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand , finger );
		    			  String rest_on_key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key ( (int) finger_resting_location.Ob1, (int) finger_resting_location.Ob2);
		    			  sim.funs.AnimatorModuleFun__Set_Finger_Rest_On_A_Place(hand, finger,rest_on_key);
		    		  }
		    	  }
		      }

		      //Animator3D initialization
		      if(sim.vars.animator3DModule.Show_Animator3D ){
		        itr_Objects = sim.vars.world3DTemplate.World.Objects.entrySet().iterator();
		        while(itr_Objects.hasNext()){
		          Entry<String, Object> an_object = itr_Objects.next();
		
		          if(an_object.getValue() instanceof World3D_DriverCar){
		            World3D_DriverCar driver_car = (World3D_DriverCar)an_object.getValue();
		            World3D_Road the_start_road = (World3D_Road)sim.vars.world3DTemplate.World.Objects.get(driver_car.Start_Road_World3D_ID);
		            
		            
		            //System.out.println("TODO Event_06_Animator_Initialization Animator3D DriverCar ObjectProperties");
		            //probably here just need to set the Java3D view location
		            
		            
		//            ObjectProperties properties = new ObjectProperties();
		//            properties.ModelID = sim.vars.animator3DModule.DriverCar_Model_ID;
		//            properties.ScaleX = sim.vars.animator3DModule.DriverCar_Shade_Scale_X;
		//            properties.ScaleZ = sim.vars.animator3DModule.DriverCar_Shade_Scale_Z;
		//            
		//            properties.X = driver_car.Vehicle_Basic.Loc_X;
		//            properties.Y = driver_car.Vehicle_Basic.Loc_Y + driver_car.Vehicle_Basic.Camera_Height;
		//            properties.Z = driver_car.Vehicle_Basic.Loc_Z;
		//            properties.Pitch = driver_car.Vehicle_Basic.Pitch;
		//            properties.Yaw = driver_car.Vehicle_Basic.Yaw;
		//            properties.Roll = driver_car.Vehicle_Basic.Roll;
		//            
		//            String ojbect_id = (String)Animator3D.Create3DObject(properties);
		//            driver_car.Vehicle_Basic.Animator3D_Object_ID = ojbect_id;
		
		      //Animator3D.Hide(ojbect_id );      
		
		            //in Java version, currently only assume driver car start at (0, 0, 0)
		            QnactrSimulation.taskVisualization3D.viewTranslationY(driver_car.Vehicle_Basic.Camera_Height);
		            
		//            Animator3D.AttachCameraToObject(driver_car.Vehicle_Basic.Animator3D_Object_ID);
		          } //end of World3D_DriverCar
		          else if(an_object.getValue() instanceof World3D_OtherCar){
		            World3D_OtherCar other_car = (World3D_OtherCar)an_object.getValue();
		            World3D_Road the_start_road = (World3D_Road)sim.vars.world3DTemplate.World.Objects.get(other_car.Start_Road_World3D_ID);
		            
		//TODO 
		            System.out.println("TODO Event_06_Animator_Initialization Animator3D OtherCar ObjectProperties");
		//            ObjectProperties properties = new ObjectProperties();
		//            properties.ModelID = sim.vars.animator3DModule.OtherCar_Model_ID;
		//            properties.X = other_car.Vehicle_Basic.Loc_X;
		//            properties.Y = other_car.Vehicle_Basic.Loc_Y ;
		//            properties.Z = other_car.Vehicle_Basic.Loc_Z;
		//            properties.Pitch = other_car.Vehicle_Basic.Pitch;
		//            properties.Yaw = other_car.Vehicle_Basic.Yaw;
		//            properties.Roll = other_car.Vehicle_Basic.Roll;
		//            properties.ScaleX = sim.vars.animator3DModule.OtherCar_Scale;
		//            properties.ScaleY = sim.vars.animator3DModule.OtherCar_Scale;
		//            properties.ScaleZ = sim.vars.animator3DModule.OtherCar_Scale;
		//            
		//            String ojbect_id = (String)Animator3D.Create3DObject(properties);
		//            Animator3D.SetColor( ojbect_id, Color.Blue ) ;
		//            other_car.Vehicle_Basic.Animator3D_Object_ID = ojbect_id;
		      //System.out.println("Event_06_Animator_Initialization other_car: " + properties.X + " " + properties.Y + " " + properties.Z);
		          } //end of World3D_OtherCar
		          else if (an_object.getValue() instanceof World3D_Road){
		            // visualize all lanes for all fragments
		            World3D_Road road = (World3D_Road) an_object.getValue();
		            ArrayList<Double> fragment_distance_points = new ArrayList<Double>();
		            
		            Iterator <Entry <Double,World3D_Road_Fragment> > itr_Fragments = road.Fragments.entrySet().iterator();
		            while (itr_Fragments.hasNext()){
		              Entry<Double, World3D_Road_Fragment> an_entry = itr_Fragments.next();
		              fragment_distance_points.add( (double)an_entry.getKey() );
		            }
		            Collections.sort(  fragment_distance_points  );
		            
		            Iterator <Double> itr_fragment_distance_points = fragment_distance_points.iterator();
		            while(itr_fragment_distance_points.hasNext()){
		              Double fragment_start_distance = itr_fragment_distance_points.next();
		              World3D_Road_Fragment a_fragment = (World3D_Road_Fragment)road.Fragments.get(fragment_start_distance);
		              double center_start_x = a_fragment.Start_Loc_X;
		              double center_start_z = a_fragment.Start_Loc_Z;
		              double center_end_x = a_fragment.End_Loc_X;
		              double center_end_z = a_fragment.End_Loc_Z;
		              double start_angle_radian = a_fragment.Start_Heading_Angle / 180.0 * Math.PI;
		              double end_angle_radian = ( a_fragment.Start_Heading_Angle + a_fragment.Turn_Angle ) / 180.0 * Math.PI;
		              
		              double start_x_offset = road.Lane_Width/2.0 * Math.cos(start_angle_radian); 
		              double start_z_offset = road.Lane_Width/2.0 * Math.sin(start_angle_radian); //an angle, in radians
		                
		              double center_lane_left_edge_start_x = center_start_x - start_x_offset; 
		              double center_lane_left_edge_start_z = center_start_z + start_z_offset;
		              double center_lane_right_edge_start_x = center_start_x + start_x_offset; 
		              double center_lane_right_edge_start_z = center_start_z - start_z_offset;
		              
		              double end_x_offset = road.Lane_Width/2.0 * Math.cos(end_angle_radian);
		              double end_z_offset = road.Lane_Width/2.0 * Math.sin(end_angle_radian);
		              
		              double center_lane_left_edge_end_x = center_end_x - end_x_offset; 
		              double center_lane_left_edge_end_z = center_end_z + end_z_offset;
		              double center_lane_right_edge_end_x = center_end_x + end_x_offset; 
		              double center_lane_right_edge_end_z = center_end_z - end_z_offset;
		             
		              if(!road.hide){ // add center lane lelft edge
		                
		          //#region experiment_driving_and_comprehension                 
		                
		                if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals(  "experiment_driving_and_comprehension")){
		                  LinkedList<String> animator3d_IDs = sim.funs.AnimatorModuleFun__Animator3D_Add_Dashed_Line_XZ_Plane( center_lane_left_edge_start_x, center_lane_left_edge_start_z,     center_lane_left_edge_end_x, center_lane_left_edge_end_z  );
		                  if(animator3d_IDs.size() > 0){
		                    for(String an_id : animator3d_IDs){
		                      a_fragment.Animator3D_IDs.put("0_left" + "_dashed_" + an_id, an_id );
		                      //hide end points in AnimatorModuleFun__Animator3D_Add_Dashed_Line_XZ_Plane
		                    }
		                  }
		                }
		          //#endregion      
		                else{ 
		//                  System.out.println("TODO Event_06_Animator_Initialization Animator3D");
		//                  String center_left_edge_animator3d_id = Animator3D.Create3DLine(center_lane_left_edge_start_x, 0.0, center_lane_left_edge_start_z,     center_lane_left_edge_end_x, 0.0, center_lane_left_edge_end_z );
		//                  a_fragment.Animator3D_IDs.put("0_left", center_left_edge_animator3d_id );
		//                  Animator3D.Hide3DLineEndPoint(center_left_edge_animator3d_id);
		                  QnactrSimulation.taskVisualization3D.draw3DLine(center_lane_left_edge_start_x, -0.0, center_lane_left_edge_start_z,     center_lane_left_edge_end_x, -0.0, center_lane_left_edge_end_z ); //X Z the same, Y need to be additive inversed
		                  
		                  
		                }
		              }
		              
		              if(!road.hide){ // add center lane right edge
		//                System.out.println("TODO Event_06_Animator_Initialization Animator3D");
		//                String center_right_edge_animator3d_id = Animator3D.Create3DLine(center_lane_right_edge_start_x, 0.0, center_lane_right_edge_start_z,   center_lane_right_edge_end_x, 0.0, center_lane_right_edge_end_z);
		//                a_fragment.Animator3D_IDs.put("0_right", center_right_edge_animator3d_id);
		//                Animator3D.Hide3DLineEndPoint(center_right_edge_animator3d_id);
		                QnactrSimulation.taskVisualization3D.draw3DLine(center_lane_right_edge_start_x, -0.0, center_lane_right_edge_start_z,   center_lane_right_edge_end_x, -0.0, center_lane_right_edge_end_z); //X Z the same, Y need to be additive inversed
		              }
		              
		              if( road.Lane_Num_Left > 0 ){
		                int i;
		                for(i = 1 ; i <= road.Lane_Num_Left ; i++) {
		                  double the_lane_left_edge_start_x = center_start_x - (1 + 2*i) * start_x_offset; 
		                  double the_lane_left_edge_start_z = center_start_z + (1 + 2*i) * start_z_offset;
		                  //double the_lane_right_edge_start_x = center_start_x + (1 + 2*i) * start_x_offset; 
		                  //double the_lane_right_edge_start_z = center_start_z - (1 + 2*i) * start_z_offset;
		                  
		                  double the_lane_left_edge_end_x = center_end_x - (1 + 2*i) * end_x_offset; 
		                  double the_lane_left_edge_end_z = center_end_z + (1 + 2*i) * end_z_offset;
		                  //double the_lane_right_edge_end_x = center_end_x + (1 + 2*i) * end_x_offset; 
		                  //double the_lane_right_edge_end_z = center_end_z - (1 + 2*i) * end_z_offset;
		                  
		                  if(!road.hide){
		//                    System.out.println("TODO Event_06_Animator_Initialization Animator3D");
		//                    String the_left_edge_animator3d_id = Animator3D.Create3DLine(the_lane_left_edge_start_x, 0.0, the_lane_left_edge_start_z,     the_lane_left_edge_end_x, 0.0, the_lane_left_edge_end_z );
		//                    a_fragment.Animator3D_IDs.put("-" + i.ToString() + "_left", the_left_edge_animator3d_id );
		//                    Animator3D.Hide3DLineEndPoint(the_left_edge_animator3d_id);
		                    
		                    QnactrSimulation.taskVisualization3D.draw3DLine(the_lane_left_edge_start_x, -0.0, the_lane_left_edge_start_z,     the_lane_left_edge_end_x, -0.0, the_lane_left_edge_end_z ); //X Z the same, Y need to be additive inversed
		                  }
		                  //String the_right_edge_animator3d_id = Animator3D.Create3DLine(the_lane_right_edge_start_x, 0.0, the_lane_right_edge_start_z,   the_lane_right_edge_end_x, 0.0, the_lane_right_edge_end_z);
		                  //a_fragment.Animator3D_IDs.put(?? + "_right", the_right_edge_animator3d_id);   
		                }
		              }//end of adding left other lanes
		              
		              if (road.Lane_Num_Right > 0){
		                int i;
		                for(i = 1 ; i <= road.Lane_Num_Left ; i++) {
		                  //double the_lane_left_edge_start_x = center_start_x - (1 + 2*i) * start_x_offset; 
		                  //double the_lane_left_edge_start_z = center_start_z + (1 + 2*i) * start_z_offset;
		                  double the_lane_right_edge_start_x = center_start_x + (1 + 2*i) * start_x_offset; 
		                  double the_lane_right_edge_start_z = center_start_z - (1 + 2*i) * start_z_offset;
		                  
		                  //double the_lane_left_edge_end_x = center_end_x - (1 + 2*i) * end_x_offset; 
		                  //double the_lane_left_edge_end_z = center_end_z + (1 + 2*i) * end_z_offset;
		                  double the_lane_right_edge_end_x = center_end_x + (1 + 2*i) * end_x_offset; 
		                  double the_lane_right_edge_end_z = center_end_z - (1 + 2*i) * end_z_offset;
		                  
		                  //String the_left_edge_animator3d_id = Animator3D.Create3DLine(the_lane_left_edge_start_x, 0.0, the_lane_left_edge_start_z,     the_lane_left_edge_end_x, 0.0, the_lane_left_edge_end_z );
		                  //a_fragment.Animator3D_IDs.put(?? + "_left", the_left_edge_animator3d_id );
		                  
		                  if(!road.hide){
		//                    System.out.println("TODO Event_06_Animator_Initialization Animator3D");
		//                    String the_right_edge_animator3d_id = Animator3D.Create3DLine(the_lane_right_edge_start_x, 0.0, the_lane_right_edge_start_z,   the_lane_right_edge_end_x, 0.0, the_lane_right_edge_end_z);
		//                    a_fragment.Animator3D_IDs.put(i.ToString() + "_right", the_right_edge_animator3d_id); 
		//                    Animator3D.Hide3DLineEndPoint(the_right_edge_animator3d_id);
		                    QnactrSimulation.taskVisualization3D.draw3DLine(the_lane_right_edge_start_x, -0.0, the_lane_right_edge_start_z,   the_lane_right_edge_end_x, -0.0, the_lane_right_edge_end_z ); //X Z the same, Y need to be additive inversed
		                   }
		                }
		              }//end of adding right other lanes
		            }
		          } //end of World3D_Road
          
		          else if(an_object.getValue() instanceof World3D_Point){
		        	  World3D_Point the_point = (World3D_Point)an_object.getValue();
	            
		        	  if (the_point.Type.equals( "customized-local-point")){
	//TODO
		        		  System.out.println("TODO Event_06_Animator_Initialization ObjectProperties");
	              
		//              ObjectProperties properties = new ObjectProperties();
		//              properties.ModelID = sim.vars.animator3DModule.Point_Model_ID;
		//              
		//              Three_Tuple global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(the_point.On_Road_World3D_ID, the_point.Distance_From_Start, the_point.Distance_Lateral_To_Center_Lane_Center, 0.0);
		//              the_point.Loc_X = (double) global_para.Ob1;
		//              the_point.Loc_Z = (double) global_para.Ob2;
		//                    
		//              properties.X = the_point.Loc_X;
		//              properties.Y = the_point.Loc_Y - the_point.Size_Y / 2.0;  // Loc_Y should be 0 as initially. Animator3D by default put the object's bottom at Y = 0 level, so this sets Y at 0.
		//              properties.Z = the_point.Loc_Z;
		//              
		//              double size_factor = 1.0 / 3.0 ; // for Sphere3Foot object
		//              
		//              properties.ScaleX = the_point.Size_X * size_factor ;
		//              properties.ScaleY = the_point.Size_Y * size_factor ;
		//              properties.ScaleZ = the_point.Size_Z * size_factor ;
		//              
		//              String ojbect_id = (String)Animator3D.Create3DObject(properties);
		//              Animator3D.SetColor( ojbect_id, the_point.Color ) ;
		//              the_point.Animator3D_Object_ID = ojbect_id;
		        	  }
		        	  else if(the_point.Type.length() >= 9 && the_point.Type.substring(0,9).equals( "far-point")){
	//TODO
		        		  System.out.println("TODO Event_06_Animator_Initialization ObjectProperties");
	              
	//              ObjectProperties properties = new ObjectProperties();
	//              properties.ModelID = sim.vars.animator3DModule.Point_Model_ID;
	//              
	//              Three_Tuple global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(the_point.On_Road_World3D_ID, the_point.Distance_From_Start, the_point.Distance_Lateral_To_Center_Lane_Center, 0.0);
	//              the_point.Loc_X = (double) global_para.Ob1;
	//              the_point.Loc_Z = (double) global_para.Ob2;
	//                    
	//              properties.X = the_point.Loc_X;
	//              properties.Y = the_point.Loc_Y;  // should be 0 as initially, Animator3D by default put the object's bottom at Y = 0 level, so this far-point's bottom is at Y = 0 level.
	//              properties.Z = the_point.Loc_Z;
	//              
	//              properties.ScaleX = sim.vars.animator3DModule.Far_Point_Scale;
	//              properties.ScaleY = sim.vars.animator3DModule.Far_Point_Scale;
	//              properties.ScaleZ = sim.vars.animator3DModule.Far_Point_Scale;
	//              
	//              String ojbect_id = (String)Animator3D.Create3DObject(properties);
	//              Animator3D.SetColor( ojbect_id, Color.Red ) ;
	//              the_point.Animator3D_Object_ID = ojbect_id;
		        	  }
		            else{
		              System.out.println("Error! Event_06_Animator_Initialization has undefined Point type: " + the_point.Type);
		              SimSystem.abort();
		            }
		          } //end of World3D_Point
          
		          else if(an_object.getValue() instanceof World3D_Line_Segment){
		            World3D_Line_Segment the_line_seg = (World3D_Line_Segment)an_object.getValue();
		            
		            if (the_line_seg.Type.equals( "customized-local-line-segment")){
		//TODO
		              System.out.println("TODO Event_06_Animator_Initialization Animator3D");
		//              String ojbect_id = (String)Animator3D.Create3DLine( the_line_seg.P1_Loc_X, the_line_seg.P1_Loc_Y, the_line_seg.P1_Loc_Z, the_line_seg.P2_Loc_X, the_line_seg.P2_Loc_Y, the_line_seg.P2_Loc_Z);
		//              Animator3D.SetColor( ojbect_id, the_line_seg.Color) ;
		//              the_line_seg.Animator3D_Object_ID = ojbect_id;
		            }
		            else{
		              System.out.println("Error! Event_06_Animator_Initialization has undefined World3D_Line_Segment the_line_seg.Type: " + the_line_seg.Type );
		              SimSystem.abort();
		            }
		          } //end of World3D_Line_Segment
		          else{
		            System.out.println("Error! Event_06_Animator_Initialization has undefined world3d object type"); 
		            SimSystem.abort();
		          }
		        }
		      }
      //#region customized Animator Ojbects
        //#region for Space 6DOF speed-control demo  sp-driv
		      if( sim.vars.world3DTemplate.Method_Object != null && (sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method)){
		    	  World3D_Template_Driving_Method  driving_method_pointer =  sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
		    	  if(driving_method_pointer != null && (driving_method_pointer.Who_Drive.toLowerCase().equals( "model-6dof-speed") || driving_method_pointer.Who_Drive.toLowerCase().equals( "human-6dof-speed" )) ){ 

//TODO
		    		  System.out.println("TODO Event_06_Animator_Initialization ObjectProperties");
            
//            ObjectProperties properties6DOF_Target_Background = new ObjectProperties();
//            properties6DOF_Target_Background.ModelID = "Target_Background";
//            
//            if(sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation == null){
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation = new Six_Tuple();
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob1 = 0;
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob2 = 0;
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob3 = 1000;
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob4 = 0;
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob5 = 0;
//              sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob6 = 0;
//            }
//            
//            double loc_X = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob1;
//            double loc_Y = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob2;
//            double loc_Z = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob3;
//            //double pitch = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob4;
//            //double yaw = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob5;
//            //double roll = (double)sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob6;
//            
//            properties6DOF_Target_Background.X = loc_X;  //move center X
//            properties6DOF_Target_Background.Y = loc_Y - 35;  // move center Y
//            properties6DOF_Target_Background.Z = loc_Z;  // move center Z, 1 Animator unit = 1 cm for real space 6DOF experiment.
//            
//            properties6DOF_Target_Background.ScaleX = 0.333333;  // change "Target_Background" to the desired size
//            properties6DOF_Target_Background.ScaleY = 0.333333;
//            properties6DOF_Target_Background.ScaleZ = 0.333333;
//            
//            //ToDo, here, set rotation.
//            properties6DOF_Target_Background.Pitch = 0;
//            properties6DOF_Target_Background.Yaw = 0;
//            properties6DOF_Target_Background.Roll = 0;
//            
//            Animator3D.CreateImage( properties6DOF_Target_Background ) ;
//        
//        
//        
//            ObjectProperties properties6DOF_Target_Cross = new ObjectProperties();
//            properties6DOF_Target_Cross.ModelID = "Target_Cross";
//            
//            
//            //ToDo, here, in fact, must set Target cross XYZ based on ( 0, 0, -70) rotate (pitch, yaw, roll) then add to target XYZ.
//            properties6DOF_Target_Cross.X = loc_X;  //move center X 
//            properties6DOF_Target_Cross.Y = loc_Y - 35;  // move center Y 
//            properties6DOF_Target_Cross.Z = loc_Z - 70 ;  // move center Z, distance between background and cross is 70 cm
//            
//            
//            
//            properties6DOF_Target_Cross.ScaleX = 0.333333;  // change "Target_Background" to the desired size
//            properties6DOF_Target_Cross.ScaleY = 0.333333;
//            properties6DOF_Target_Cross.ScaleZ = 0.333333;
//            
//            //ToDo, here, set rotation.
//            properties6DOF_Target_Cross.Pitch = 0;
//            properties6DOF_Target_Cross.Yaw = 0;
//            properties6DOF_Target_Cross.Roll = 0;
//            Animator3D.CreateImage( properties6DOF_Target_Cross ) ;
            
            
            /*
            ObjectProperties properties6DOF_Target_Background = new ObjectProperties();
            properties6DOF_Target_Background.ModelID = "Target_Background"; //
            
            properties6DOF_Target_Background.X = 4.5;  //move center to X = 0. this is a bug. I reported. They should fix it.
            properties6DOF_Target_Background.Y = -50.3;  // move center to Y = 0
            properties6DOF_Target_Background.Z = 1000;  // 1000 Animator unit = 10 m for real space 6DOF experiment.
            
            properties6DOF_Target_Background.ScaleX = 0.333333;  // change "Target_Background" to the desired size,  0.7 m x 0.7 m. 
            properties6DOF_Target_Background.ScaleY = 0.333333;
            properties6DOF_Target_Background.ScaleZ = 0.333333;
            Animator3D.CreateImage( properties6DOF_Target_Background ) ;
        
        
        
            ObjectProperties properties6DOF_Target_Cross = new ObjectProperties();
            properties6DOF_Target_Cross.ModelID = "Target_Cross";
            
            properties6DOF_Target_Cross.X = 4.5;  //move center to X = 0.
            properties6DOF_Target_Cross.Y = -50.3;  // move center to Y = 0
            properties6DOF_Target_Cross.Z = properties6DOF_Target_Background.Z - 70 ;  // 
            
            properties6DOF_Target_Cross.ScaleX = 0.333333;  // change "Target_Background" to the desired size
            properties6DOF_Target_Cross.ScaleY = 0.333333;
            properties6DOF_Target_Cross.ScaleZ = 0.333333;
            Animator3D.CreateImage( properties6DOF_Target_Cross ) ;
            */
          
          
		    	  }
		      }




        //#endregion

      //#endregion

        
        
      //System.out.println("Event_06_Animator_Initialization DONE");
      
      
      //Event_07_Extra_Predefined_Model_Setup
       // System.out.println("Clock " + SimSystem.clock() + ", Event_07_Extra_Predefined_Model_Setup");
        
        
//        System.out.println("Clock " + SimSystem.clock() + ", Event_07_Extra_Predefined_Model_Setup has additional debug codes.");
//        sim.funs.ProductionModuleFun__Print_All_Productions();
        
		      if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_medical_decision_making")){

//          System.out.println("TODO Event_07_Extra_Predefined_Model_Setup MedicalExp");
          
//         at the start of simulation
//             if (!MedicalExp.FormManager.theStart != null && MedicalExp.FormManager.theStart.stage.equals( "" ))
//             {
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-update-retrieval-imaginal", "goal-2");
//              sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-update-recreate-imaginal", "goal-2");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-question-retrieval-imaginal", "goal-2");
//              sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-question-recreate-imaginal", "goal-2");
//            
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-sound", "goal-2");
//              // // no need in final model// sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-update", "goal-2");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-update-patient", "goal-2");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-question", "goal-2");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-hear-question-patient", "goal-2");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("memory-rehearse-memory", "goal-2");
//          
//          
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("decision-attending-test-retrieval-imaginal", "goal");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("decision-attending-test-recreate-imaginal", "goal");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("decision-decide-number-retrieval-imaginal", "goal");
//            sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("decision-decide-number-recreate-imaginal", "goal");
//            
//              // // no need in final model// sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("decision-rehearse-memory", "goal");
//          
//                 switch (MedicalExp.FormManager.theStart.stage)
//                 {
//                     case "single-decision":
//                         {
//                             //start medical single practice/simulation
//                             MedicalExp.FormManager.theMedicalTask.initializeTrial( MedicalExp.FormManager.theMedicalTask.sessionIsDual, MedicalExp.FormManager.theMedicalTask.sessionIsPractice); 
//                             break;
//                         }
//                     case "single-monitor":
//                         {
//                             MedicalExp.FormManager.theMonitoringTask.initializeTrial(MedicalExp.FormManager.theMonitoringTask.sessionIsDual, MedicalExp.FormManager.theMonitoringTask.sessionIsPractice);
//
//                             break;
//                         }
//                     case "single-memory":
//                         {
//                             MedicalExp.FormManager.theMemoryTask.initializeTrial(MedicalExp.FormManager.theMemoryTask.sessionIsDual, MedicalExp.FormManager.theMemoryTask.sessionIsPractice);
//                             break;
//                         }
//                     case "dual-decision-monitor":
//                         {
//                              MedicalExp.FormManager.theMedicalTask.initializeTrial( MedicalExp.FormManager.theMedicalTask.sessionIsDual, MedicalExp.FormManager.theMedicalTask.sessionIsPractice); 
//                  MedicalExp.FormManager.theMonitoringTask.initializeTrial(MedicalExp.FormManager.theMonitoringTask.sessionIsDual, MedicalExp.FormManager.theMonitoringTask.sessionIsPractice);
//                             break;
//                         }
//                     case "dual-decision-memory":
//                         {
//                              MedicalExp.FormManager.theMedicalTask.initializeTrial( MedicalExp.FormManager.theMedicalTask.sessionIsDual, MedicalExp.FormManager.theMedicalTask.sessionIsPractice); 
//                  MedicalExp.FormManager.theMemoryTask.initializeTrial(MedicalExp.FormManager.theMemoryTask.sessionIsDual, MedicalExp.FormManager.theMemoryTask.sessionIsPractice);
//                             break;
//                         }
//                     default:
//                         {
//                             System.out.println("Error! Event_07_Extra_Predefined_Model_Setup has undefined stage case: " + MedicalExp.FormManager.theStart.stage);
//                             
//                             break;
//                         }
//                 }
//             }
		      }
        
	        if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_driving_and_comprehension") ){
	          //specifying the rules that require follow up processing, as in the filtering discipline
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("heard-word-sound-retrieve-meaning", "goal");  // +retrieval>
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("heard-low-beep", "goal");  // +imaginal>
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("heard-high-beep", "goal");  // +retrieval>
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("start-recognition-1", "goal");  // +imaginal>
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("start-recognition-2", "goal");  // +imaginal>
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-subject-match-yes", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-subject-match-no", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-aux-match-yes-1", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-aux-match-no-1", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-aux-match-yes-2", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("find-next-sound-phase-probe-aux-match-no-2", "goal");  // +aural-location>, listening to important aural info (probe sentence)
	          
	          sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.put("drive-control-process-near-attend-far", "goal-2");  //drive-control-process-far will always following this without interruption by T2, used here to achieve the same effect as visual-motor path way not blocked by the limited production module resource, as a workaround
	        }
            
            // end of adding Clock = 0 Scenario Events
        
	        if(QnactrSimulation.computeUtilization){
	          //Recurrent_01_Compute_Utilization
	          //repeat for ever, every 1 second
	          Entity Temp_Entity_Recurrent_01 = sim.funs.createEntity( "Recurrent Event" , "Recurrent Event", "Recurrent Event", "Recurrent Event 1", 0.0);
	          Temp_Entity_Recurrent_01.Direct_Cast_Delay = 1.0; 
	        }
        
            break;
            
          case Timing:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            return 0.0;  
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break; 
        }
        break;
        
        
      
      case firsttriggerentity:

        switch (ServiceStage){
          
          case Release:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
            
          case Beginning:
            //JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            
            Entity.hmiID = Integer.toString(sim.ID);
            
            if( sim.vars.world3DTemplate.World.Object_Num > 0 
            		|| sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_opends" )
            		|| sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_torcs" )
            		|| sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "unity_tangtang_2015" )
            		
            		){ //initialize World3D_Cyclic_Refresh
              Entity Temp_Entity = sim.funs.createEntity( "World3D Cyclic Refresh" , "", "", "", 0.0);
              //              Entity Temp_Entity = new Entity();  
              //              Temp_Entity.ID = "501"; //World3D_Cyclic_Refresh
              //              Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
              //              QnactrSimulation.entityNumber++;
              
            }
            
            break;
            
          case Ending:
            
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); 
            
            Entity.Entity_Type = "Display Initialization";
            
            QnactrSimulation.simStartRealClockTime = System.currentTimeMillis();
 
            break;
            
          case Timing:
            //            JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
            
            return 0.0;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;  
        }
        break;
        
     
      case audiodisplay:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
            
          case Beginning:
            Entity.Time_Computed = false;
            break;
            
          case Ending:
            
            //GlobalUtilities.popUpMessage( Entity.Entity_Type + " " +  Entity.Scheduled_Task_Enter_Clock_Time);
            
            if( Entity.Entity_Type.equals( "Audio Display Onset") ){ //may come from different places like audio display schedule or speech execution (for subvocalize)
              
              sim.funs.ProgramUtilitiesFun__Output_Foot_Results_Txt(Double.toString(GlobalUtilities.round(SimSystem.clock(),3)) + "\t" + "Audio" );
              
              Chunk temp_chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              temp_chunk.Creation_Time = SimSystem.clock();
              sim.vars.audioDisplay.Audicon.addLast(temp_chunk);
              sim.vars.audioDisplay.Ongoing_Sound.addLast(temp_chunk);
              
              sim.vars.sim.vars.visualization__Audio_Display = sim.funs.DeviceModuleFun__Get_Audio_Display (); //this is the old visualization method
              sim.funs.AnimatorModuleFun__Add_Audicon( temp_chunk ); //this is the new visualization method
              
              Entity.From = "Audio Display"; //tag from and to
              Entity.To = "Audio Module";
              Entity.Entity_Type = "Audio Display Change Notice";  
              //GlobalUtilities.popUpMessage("Audio display entity.chunk: "  );	
              //sim.funs.ChunkFun__Popout_Message_Show_Chunk_Contents(Entity.Chunk);
              
            }
            else if (Entity.Entity_Type.equals( "Audio Display Offset")) {//remove the sound from ongoing list, and start sound decay tracking
              //GlobalUtilities.popUpMessage( sim.vars.audioDisplay.Ongoing_Sound.Count );
              int temp_id = sim.funs.AudioModuleFun__Find_The_Ongoing_Sound_ID_By_Chunk_Name(Entity.Chunk.Chunk_Name);
              sim.vars.audioDisplay.Ongoing_Sound.remove( sim.funs.ProgramUtilitiesFun__LinkedList_Get_i_th_Chunk_Pointer(sim.vars.audioDisplay.Ongoing_Sound,  temp_id)) ;
              //GlobalUtilities.popUpMessage( sim.vars.audioDisplay.Ongoing_Sound.Count );
              sim.vars.visualization__Audio_Display = sim.funs.DeviceModuleFun__Get_Audio_Display ();
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.audioModule.Audicon_Decay_Time_Tracking_Table, Entity.Chunk.Chunk_Name , SimSystem.clock()  );
              Entity.To = "";
              Entity.Entity_Type = "";  
              Entity.Trash = true; //ACT-R seems not sending audio buffer stuffing for audio display offset
            }
            
            
            Entity.Scheduled_Task_Enter_Clock_Time = (double) 0.0;	
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Audicon();
            break;
            
          case Timing:
            //GlobalUtilities.popUpMessage(Clock );
            //GlobalUtilities.popUpMessage(Entity.Scheduled_Task_Enter_Clock_Time );
            if( GlobalUtilities.round(SimSystem.clock(),3 ) > GlobalUtilities.round(Entity.Scheduled_Task_Enter_Clock_Time ,3)) { //otherwise, if the Scheduled time was just assigned with the Clock time, the new Clock time, though is still the same, will be greater than the scheduled time
              //GlobalUtilities.popUpMessage( GlobalUtilities.round(Clock , 15 ) == GlobalUtilities.round(Entity.Scheduled_Task_Enter_Clock_Time , 15 ));
              //GlobalUtilities.popUpMessage("audio display Clock > Entity.Scheduled_Task_Enter_Clock_Time. Chunk_Name: " + Entity.Chunk.Chunk_Name + " Clock: " + Clock + " Scheduled time: " + Entity.Scheduled_Task_Enter_Clock_Time);
              Entity.Time_Computed = true;
              return 0.0;
            }
            else {
              Entity.Time_Computed = true;
              return GlobalUtilities.round((Entity.Scheduled_Task_Enter_Clock_Time - SimSystem.clock()),3);
            }
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case audiomodule:
        switch (ServiceStage){
          case Release:
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            if(itemsInTask >= 2) System.out.println("Audio Module has entity #: " + itemsInTask);
            return (!Entity.Trash ) ; // && (itemsInTask < 1); //(Shi Cao thinks a parallel audio module is reasonable, so the current QN has a parallel audio module.)
            
            
          case Beginning:
            
            //Entity.Time = 0.0;
            sim.funs.AudioModuleFun__Update_Audicon_Decay();
            
            Entity.Time_Computed = false;
            //audio buffer stuffing check
            if (Entity.From.equals( "Audio Display") && Entity.Entity_Type.equals( "Audio Display Change Notice")){ //if entity comes from Audio display, first check and do Audio buffer stuffing.
              
              
              // new
              if (sim.vars.audioDisplay.Audicon.size() > 0){ //only do Aural-location buffer stuffing when there is something in the audicon
                //Aural-location buffer stuffing
                //System.out.println("Audio Module, buffer stuffing");
                Entity.Entity_Type = "Audio-Location Buffer Stuffing";
                //action moved to ending effect part
              } //stuffing for all audio entities. need queueing in aural-location buffer. QN feature.
              
              else { //currently ACT-R seems do nothing but trigger the next conflict resolution if an audio entity comes but aural location buffer is full
                Entity.Entity_Type = "Audio Display Change But No Audio-Location Buffer Stuffing";
              }
              
              //old method
        			//if (sim.vars.auralLocationBuffer.Aural_Location_Buffer_Chunk.Chunk_Name.equals( "" ) && sim.vars.auralLocationBuffer.Aural_Location_Buffer_Chunk.Chunk_Type.equals( "" ) && sim.vars.audioDisplay.Audicon.Count > 0){ //only do Aural-location buffer stuffing when there is something in the audicon
        				//Aural-location buffer stuffing
        				//System.out.println("Audio Module, buffer stuffing");
        			//	Entity.Entity_Type = "Audio-Location Buffer Stuffing";
        				//action moved to ending effect part
        			//}
        			//else { //currently ACT-R seems do nothing but trigger the next conflict resolution if an audio entity comes but aural location buffer is full
        			//	Entity.Entity_Type = "Audio Display Change But No Audio-Location Buffer Stuffing";
        			//}
              
              
            }
            
            //audio +aural>
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Aural" )){
              if (!Entity.Chunk.Chunk_Type.equals( "clear" )){
                //System.out.println("Audio Module add aural chunk type: " + Entity.Chunk.Chunk_Type);	
                String aural_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "event");
                if(aural_location_chunk_name.equals( "" ) )System.out.println("Audio Module +aural> beginning effect aural_location_chunk_name  empty ToDo:");
                else{
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\tATTEND-" + Entity.Chunk.Chunk_Type + " " + aural_location_chunk_name ); 
                  sim.vars.audioModule.State_Free = false;
                }
              }
              else { //+aural> isa clear
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "CLEAR");
                sim.vars.audioModule.State_Preparation_Free = false;
              }
            }
            
            else System.out.println( "WARNING! Auido Module beginning effect has an undefined case");
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Audicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Location_Buffer_Contents();
            
            break;
            
          case Ending:
            
            sim.funs.AudioModuleFun__Update_Audicon_Decay();
            
            if(Entity.From.equals( "Audio Display") && Entity.Entity_Type.equals( "Audio-Location Buffer Stuffing")){
              if (!Entity.Chunk.Chunk_Name.equals( "" )){ //Entity.Chunk is changed from audicon to audio location event, in timing codes.
                //since aural-location buffer chunk will not be modified, it seems not necessary to rename it.
                
                //double check whether the selected buffer stuffing chunk has decayed during the content-delay time, which is a ToDo task.
                sim.funs.AudioModuleFun__Update_Audicon_Decay();
                //get all the names of audio-event chunks that belongs to the chunks currently in the audicon list
                LinkedList<String> aural_location_chunk_list = new LinkedList<String> ();
                Enumeration enum_audicon_chunk = Collections.enumeration(sim.vars.audioDisplay.Audicon);
                while(enum_audicon_chunk.hasMoreElements()){
                  Chunk audicon_chunk = (Chunk) enum_audicon_chunk.nextElement();
                  String event_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value( audicon_chunk, "event") ;
                  aural_location_chunk_list.addLast( event_name );
                }
                //System.out.println("audio module all current audicons' location event names: " + sim.funs.ProgramUtilitiesFun__LinkedListString_To_String_Show_Empty( aural_location_chunk_list) + ".  the current incoming chunk name: " + Entity.Chunk.Chunk_Name + ". location: " + sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "location"));	
                
                if ( aural_location_chunk_list.contains(Entity.Chunk.Chunk_Name) == false ) System.out.println("Audio Module buffer stuffing: the selected buffer stuffing chunk has decayed during the content-delay time, which is a ToDo task");
                else {		
                  sim.vars.auralLocationBuffer.Aural_Location_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone (Entity.Chunk);
                  //sim.funs.ChunkFun__Popout_Message_Show_Chunk_Contents(sim.vars.auralLocationBuffer.Aural_Location_Buffer_Chunk );			
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "SET-BUFFER-CHUNK AURAL-LOCATION " + sim.vars.auralLocationBuffer.Aural_Location_Buffer_Chunk.Chunk_Name + "  REQUESTED NIL"); 
                  sim.vars.auralLocationBuffer.Empty = false;
                  
                }
              }
              else{
                System.out.println("Audio Module Ending Effect audio-location chunk is empty, should print-out some trace here?");
              }
              sim.vars.audioModule.State_Free = true;	
              Entity.From = "Audio Module";
              Entity.To = "Aural-location Buffer";	
              Entity.Entity_Type = "Production Rule Firing Trigger"	;
            }
            
            else if (Entity.From.equals( "Audio Display") && Entity.Entity_Type.equals( "Audio Display Change But No Audio-Location Buffer Stuffing")){ //currently ACT-R seems do nothing but trigger the next conflict resolution if an audio entity comes but aural location buffer is full
              Entity.From = "Audio Module";
              Entity.To = "Trash";	
              Entity.Entity_Type = "Trash"	;
              Entity.Trash = true;
            }
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Aural")) { 
              if (!Entity.Chunk.Chunk_Type.equals( "clear")){
                
                //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
                //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
                //QnactrSimulation.entityNumber++;
                Entity.From = "Audio Module"; //tag from and to
                Entity.To = "Aural Buffer";
                
                String aural_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "event");
                Chunk aural_location_chunk = (Chunk) sim.vars.centralParametersModule.Chunks.get(aural_location_chunk_name); 
                
                sim.funs.ChunkFun__Set_Chunk_Slot_Value( aural_location_chunk , ":attended", "t");
                
                if(sim.vars.audioModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names.contains( aural_location_chunk.Chunk_Name)){
                  sim.vars.audioModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names.remove( aural_location_chunk.Chunk_Name );
                  if(sim.vars.audioModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names.contains( aural_location_chunk.Chunk_Name) )System.out.println("Error! audio module endEffect has aural_location_chunk.Chunk_Name still there after removal ");
                }
                else{
                  System.out.println("Error! Audio module EndEffect has sim.vars.audioModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names.Contains( aural_location_chunk.Chunk_Name) == false ");
                }		
                
                Chunk temp_chunk = new Chunk();
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "event", aural_location_chunk_name);
                
                Entity.Chunk = sim.funs.AudioModuleFun__Find_Audicon_By_Chunk_Spec(temp_chunk); //find the chunk
                
                if (Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" )){
                  sim.vars.audioModule.State_Error = true;   //need to be verifed with ACT-R: use (buffer-status)
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "??? ToDo determine what to print here"); 
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "??? ToDo determine what to print here");
                  Entity.Entity_Type = "Production Rule Firing Trigger"	;
                  //System.out.println ("Audio_Module retrieval failed.");
                } //failed
                else{
                  if (sim.vars.messageOn) System.out.println ("Audio_Module retrieval succeeded");
                  sim.vars.audioModule.State_Error = false;
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "AUDIO-ENCODING-COMPLETE " + aural_location_chunk_name); 
                  Entity.Entity_Type = "Aural Buffer's New Chunk";  
                }
                sim.vars.audioModule.State_Free = true;
              }
              else { //+aural> isa clear
                sim.vars.audioModule.State_Error = false;
                sim.vars.audioModule.State_Preparation_Free = true;
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "CHANGE-STATE LAST NONE PREP FREE");
                
              //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
                //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
                //QnactrSimulation.entityNumber++;
                Entity.From = "Audio Module"; //tag from and to
                Entity.To = "Aural Buffer";
                Entity.Entity_Type = "Aural Clear Notice";    
                Entity.Chunk = new Chunk(); 
              }
            }
            
            else {
              System.out.println( "WARNING! Audio Module ending effect has an undefined case. Entity.From: " + Entity.From + ", Entity.Entity_Type: " + Entity.Entity_Type);
              
            }
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Audicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Location_Buffer_Contents();
            
            
            
            sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Audio_Module_Changes_In_A_Second , (double)GlobalUtilities.round(SimSystem.clock(),3) , 0.0 ) ; 
            
            break;
            
          case Timing:
            
            double duration ;
            
            if(Entity.From.equals( "Audio Display") && Entity.Entity_Type.equals( "Audio-Location Buffer Stuffing")) {
              //Aural-location buffer stuffing. because it may take time, so put it here before timing
              //GlobalUtilities.popUpMessage("Audio Module buffer stuffing");	
              //ACT-R: The default preference is for any unattended item. If multiple objects meet the constraints, then one will be picked randomly
              Chunk temp_chunk = new Chunk();
              sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, ":attended", "nil" ); 
              Chunk selected_aural_location_chunk_clone = sim.funs.AudioModuleFun__Find_Aural_Location_In_Audicons_By_Chunk_Spec (temp_chunk, true); //exclude just selected locations
              sim.vars.audioModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names.addLast( selected_aural_location_chunk_clone.Chunk_Name);
              Entity.Chunk = selected_aural_location_chunk_clone;
              if (Entity.Chunk.Chunk_Name.equals( "" )){
                System.out.println("audio module beginning effect aural-location buffer stuffing cannot find aural-location chunk");
                Entity.Time_Computed = true;
                duration = 0.0;
              }
              else{
                //sim.funs.ChunkFun__Print_Chunk(Entity.Chunk);
                String  content_delay = sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk , ":content-delay");
                sim.vars.audioModule.State_Free = false;
                Entity.Time_Computed = true;
                duration = Double.parseDouble( content_delay );
              }
            }
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Aural")){
              if(!Entity.Chunk.Chunk_Type.equals( "clear")){
                String aural_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "event");
                if(aural_location_chunk_name.equals( "" ) ) { 
                  System.out.println("Audio Module +aural> Timing aural_location_chunk_name == empty ToDo:");
                  duration = 0.0;
                }
                else{
                  String timing_string  = sim.funs.ChunkFun__Get_Chunk_Slot_Value ( (Chunk)sim.vars.centralParametersModule.Chunks.get(aural_location_chunk_name), ":recode-time");
                  double timing = Double.parseDouble(timing_string);	
                  if(timing < 0.0) {
                    System.out.println("Audio Module add aural timing has timing < 0");
                    Entity.Time_Computed = true;
                    duration = 0.0;
                  }
                  else {
                    Entity.Time_Computed = true;
                    duration = timing;
                  }
                }
              }
              else{//+aural> isa clear
                Entity.Time_Computed = true;
                duration = 0.050; //ACT-R reference manual: A clear request will make the preparation state busy for 50ms.
              }
            }
            else duration = 0.0 ; 
            
            Entity.Time_Computed = true;
            
            if(duration > 0.0 ) sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Audio_Module_Changes_In_A_Second , (double)GlobalUtilities.round(SimSystem.clock(),3) , 1.0 ) ;  
            
            return duration;

          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case aurallocationbuffer:
        switch (ServiceStage){
          case Release:
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            if(itemsInTask >= 2) System.out.println("Aural-location Buffer has entity #: " + itemsInTask);
            
            int itemsInQueue = sim.funs.getNumberOfQnactrEntityInQueue(ServerName.toString());
            if(itemsInQueue > 1 ) System.out.println("Aural-location Buffer has Queue length: " + itemsInQueue);
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
            
          case Beginning:
            Entity.Time_Computed = false;
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Audicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Location_Buffer_Contents();
            break;
            
          case Ending:
            if (Entity.Entity_Type.equals( "Production Rule Firing Trigger")){
              Entity.From = "Aural-location Buffer";
              Entity.To = "Matching And Selection";}
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case auralbuffer:
        switch (ServiceStage){
          case Release:
                        
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            if(itemsInTask >= 2) System.out.println("Aural Buffer has entity #: " + itemsInTask);
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
            
          case Beginning:
            
            Entity.Time_Computed = false;
            
            if (Entity.Entity_Type.equals( "Aural Buffer's New Chunk")) {
              //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
              int j = 0;
              String old_chunk_name = Entity.Chunk.Chunk_Name;
              String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                j++;
                new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              }
              Entity.Chunk.Chunk_Name = new_chunk_name;
              if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                Entity.Chunk.Activation = (double) 0.0; //all default values
                Entity.Chunk.Creation_Time = 0.0;
                Entity.Chunk.Number_Of_Presentations = 0;
                Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
              }
              sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
              
              sim.vars.auralBuffer.Aural_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              
              if (!sim.vars.auralBuffer.Aural_Buffer_Chunk.Chunk_Name.equals( "" ) && !sim.vars.auralBuffer.Aural_Buffer_Chunk.Chunk_Type.equals( "" )){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "SET-BUFFER-CHUNK AURAL " + sim.vars.auralBuffer.Aural_Buffer_Chunk.Chunk_Name); 
              }
              else 	sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "AUDIO" + "\t\t" + "??? Aural Buffer ToDo"); 
            }
            
            if (Entity.Entity_Type.equals( "Aural Clear Notice")) {} //currently don't know what should do
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Audicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Aural_Location_Buffer_Contents();
            break;
            
          case Ending:

            if (Entity.Entity_Type.equals( "Aural Buffer's New Chunk") || Entity.Entity_Type.equals( "Production Rule Firing Trigger" ) || Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice" )) {
              Entity.From = "Aural Buffer"; //change the entity and pass it to Matching And Selection as a trigger
              Entity.To = "Matching And Selection";
              Entity.Entity_Type = "Production Rule Firing Trigger";
              
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; 
              //QnactrSimulation.entityNumber++;

            }
            else{
              System.out.println("Aural Buffer Ending effect has undefined Entity.Type: " + Entity.Entity_Type);
            }
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case declarativemodule:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return  (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString()));
           
          case Beginning:
            
            // sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order?
            
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //1. beginning effect; 2. timing; 3. ending effect
            
            if (Entity.Entity_Type.equals( "Add Retrieval")){
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "DECLARATIVE" + "\t" + "START-RETRIEVAL" + " request details: " + sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk) ); 
              
              
              
              //compute retrieve process, and store the results in two global variables.
              //GlobalUtilities.popUpMessage("Declarative Module compute retrieval time"); 
              if (sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order == 0 ) {
                //this is normal, when no test retrieval happens before . Do nothing	
              }
              else if (sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order == 1 ) { // test retrieval happened before this.
                sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order--;
                sim.vars.programGlobalVar__Rand2_Seed1 = sim.vars.programGlobalVar__Trigger_Buffer_To_Declarative_Module_Rand2_Seed1;
                sim.vars.programGlobalVar__Rand2_Seed2 = sim.vars.programGlobalVar__Trigger_Buffer_To_Declarative_Module_Rand2_Seed2;
              }
              else System.out.println("WARNING! Declarative Module beginning effect has sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order: " + sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order);
              //GlobalUtilities.popUpMessage("Declarative Module has sim.vars.programGlobalVar__ProgrammingChecker_Declarative_Module_And_Trigger_Buffer_Retrieve_A_Chunk_Order != 1");
              
              //GlobalUtilities.popUpMessage("Declarative Module Add Retrieval: " );
              //sim.funs.ChunkFun__Popout_Message_Show_Chunk_Contents ( Entity.Chunk );
              
              sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_MatchedChunkID = sim.funs.DeclarativeModuleFun__Find_Add_Retrieval_Matched_DM_Chunk_IDs_By_Chunk_Spec(Entity.Chunk);
              sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk = sim.funs.DeclarativeModuleFun__Retrieve_A_Chunk (sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_MatchedChunkID , Entity.Chunk); //the second parameter is to pass to partial matching activation computation
              
              //GlobalUtilities.popUpMessage("Declarative Module retrieved chunk: " );
              //sim.funs.ChunkFun__Popout_Message_Show_Chunk_Contents ( sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk );
              
              
        			//if (Clock > 200.4){  //debug
        			//	GlobalUtilities.popUpMessage(sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_MatchedChunkID.Count + "_" + sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk.Chunk_Name);
        			//	ProgramUtilitiesFun__Output_Trace_Txt("Declarative Module:");
        			//	sim.funs.ChunkFun__Print_Chunk (Entity.Chunk);
        			//}
              
              if (sim.vars.printingModule.v.equals( "t") && sim.vars.declarativeModule.act == true) sim.funs.DeclarativeModuleFun__Print_DM_Activation_Trace(sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_MatchedChunkID, sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk);
              sim.vars.declarativeModule.State_Free = false;
              //sim.vars.declarativeModule.State_Busy = true;
            }
            
            break;
            
          case Ending:

            //1. beginning effect; 2. timing; 3. ending effect
            if (Entity.Entity_Type.equals( "Add Retrieval")) { 
              //entity change way:
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Declarative Module"; //tag from and to
              Entity.To = "Retrieval Buffer";
              Entity.Entity_Type = "Retrieval Buffer's New Chunk";   
              Entity.Chunk = sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk; //retrieve the max activation chunk
              
              if ((Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" )) || sim.funs.DeclarativeModuleFun__Predict_Retrieval_Success (Entity.Chunk) == false){ //check retrieval fail due to not enough activation
                sim.vars.declarativeModule.State_Error = true;
                //sim.vars.retrievalBuffer.Empty = true; //moved to retrieval buffer
                Entity.Chunk = new Chunk();
                //System.out.println ("DM retrieval failed.");
              } //retrieval failed
              else{
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "DECLARATIVE" + "\t" + "RETRIEVED-CHUNK " + Entity.Chunk.Chunk_Name + ": " + sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk)); 
                sim.vars.declarativeModule.State_Error = false;
                //sim.vars.retrievalBuffer.Empty = false; //moved to retrieval buffer
                
                //place declarative finst
                String DM_chunk_name;
                if (Entity.Chunk.DM_Name_Origin.equals( "" ))	DM_chunk_name	= Entity.Chunk.Chunk_Name;
                else DM_chunk_name	= Entity.Chunk.DM_Name_Origin;
                sim.funs.DeclarativeModuleFun__Place_Declarative_Finst_On(DM_chunk_name);
                
                //sub-symbolic computation, Presentation++ after successful retrieval
                //this seems should be achieved by clear retrieval
                //DeclarativeModuleFun__Update_Chunk_Presentation(Entity.Chunk);
                
                
                
              }
              sim.vars.declarativeModule.State_Free = true;	
              //sim.vars.declarativeModule.State_Busy = false;
            }
            
            sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Declarative_Module_Changes_In_A_Second , (double)GlobalUtilities.round(SimSystem.clock(),3) , 0.0 );
            
            
            break;
            
          case Timing:

            //1. beginning effect; 2. timing; 3. ending effect
            double DM_Retrieval_Time =  0.0;
            if (Entity.Entity_Type.equals( "Add Retrieval")) {
              DM_Retrieval_Time = sim.funs.DeclarativeModuleFun__Compute_Retrieval_Time(sim.vars.programGlobalVar__DeclarativeModule_Beginning_To_Timing_And_Ending_RetrievedChunk);
              //GlobalUtilities.popUpMessage(DM_Retrieval_Time);
            }
            Entity.Time_Computed = true;
            
            if( DM_Retrieval_Time > 0.0 ) sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Declarative_Module_Changes_In_A_Second , (double)GlobalUtilities.round(SimSystem.clock(),3) , 1.0 ) ; 
            
            return DM_Retrieval_Time;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case execution:
        switch (ServiceStage){
          case Release:

            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            if (sim.vars.centralParametersModule.Use_Procedural_Resources) {
              return (!Entity.Trash ); //may take multiple entities in QN
            }
            else {
              return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); // (itemsInTask < 1);
            }
            
            
          case Beginning:
            Entity.Time_Computed = false;
            //GlobalUtilities.popUpMessage(Entity.Entity_Type);
            //if(Clock == 2.445)GlobalUtilities.popUpMessage("Execution sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name : " + sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name);
            
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            if (Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.size() > 0){ //when there is a rule matched and selected. 
              
              //old version use a parameter of rule name and find the rule from the rule pool, but when =goal-x, it cannot bind the same rule name to two different version of the rule one with =goal and the other with =goal-2
              //ProductionModuleFun__Execute_Rule (Entity.Entity_Type); //Entity.Entity_Type carries the rule name (String)
              //new version pass a Production_Rule clone as the parameter
              
              for (Production_Rule a_selected_rule : Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution){
                sim.funs.ProductionModuleFun__Execute_Rule( a_selected_rule );
                
                //new test exclusive process goal.
                if(sim.vars.productionModule.Filtering_Follow_Up_Rule_Goal_Table.containsKey( a_selected_rule.Rule_Name )){
                  //this rule is one that requires follow up processing
                  
                  LinkedList<String> this_rule_process_goals = (LinkedList<String>) sim.vars.productionModule.Rule_Process_Goal_Buffer_Table.get(a_selected_rule.Rule_Name);
                  if( this_rule_process_goals.size() == 0 ) System.out.println("Execution to do 0");
                  else if( this_rule_process_goals.size() > 1) System.out.println("Execution to do 1");
                  else{ // == 1
                    if(sim.vars.productionModule.Focus_One_Goal_Name.equals( "" ) && !this_rule_process_goals.getFirst().equals( "") )sim.funs.ProgramUtilitiesFun__Output_Trace_Txt( "exclusive process goal activated: " + this_rule_process_goals.getFirst());
                    sim.vars.productionModule.Focus_One_Goal_Name = this_rule_process_goals.getFirst();
                  }
                }
                else{ // this rule is not one that requires follow up processing
                  if(!sim.vars.productionModule.Focus_One_Goal_Name.equals( "" ) )sim.funs.ProgramUtilitiesFun__Output_Trace_Txt( "exclusive process goal deactivated");
                  sim.vars.productionModule.Focus_One_Goal_Name = "";
                }
                
                
                
                sim.vars.programGlobalVar__ProductionModule_Currently_Processing_Rules_Name_List.remove(a_selected_rule.Rule_Name);
                //add resources back.  because there may be a match at time 0.050 and another at 0.075
                if (sim.vars.centralParametersModule.Use_Procedural_Resources) {
                  sim.vars.productionModule.Processor_Low_Level_Remaining_Capacity  			+= a_selected_rule.Num_Low_Level_Requests;
                  sim.vars.productionModule.Processor_High_Level_Remaining_Capacity 			+= a_selected_rule.Num_High_Level_Requests;
                  sim.vars.productionModule.Processor_Aural_Action_Remaining_Number 			+= a_selected_rule.Num_Aural_Action;
                  sim.vars.productionModule.Processor_Aural_Location_Action_Remaining_Number  += a_selected_rule.Num_Aural_Location_Action ;
                  sim.vars.productionModule.Processor_Goal_Action_Remaining_Number  			+= a_selected_rule.Num_Goal_Action ;
                  sim.vars.productionModule.Processor_Imaginal_Action_Remaining_Number  		+= a_selected_rule.Num_Imaginal_Action ;
                  sim.vars.productionModule.Processor_Manual_Action_Remaining_Number 			+= a_selected_rule.Num_Manual_Action ;
                  sim.vars.productionModule.Processor_Retrieval_Action_Remaining_Number 		+= a_selected_rule.Num_Retrieval_Action ;
                  sim.vars.productionModule.Processor_Visual_Action_Remaining_Number 			+= a_selected_rule.Num_Visual_Action ;
                  sim.vars.productionModule.Processor_Visual_Location_Action_Remaining_Number += a_selected_rule.Num_Visual_Location_Action ;
                  sim.vars.productionModule.Processor_Vocal_Action_Remaining_Number  			+= a_selected_rule.Num_Vocal_Action ;
                }
                sim.vars.visualization__Last_Executed = "Clock: " + SimSystem.clock() + ". " + a_selected_rule.Rule_Name + "\n" + sim.funs.ProductionModuleFun__Get_Rule_Contents_In_String(a_selected_rule);
              }
              
              // incoming Entity becomes the trigger of the next production rule matching
              Entity.From = "Execution";
              Entity.To = "Matching And Selection";
              Entity.Entity_Type = "Production Rule Firing Trigger";
              Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution = null;
              
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; 
              //QnactrSimulation.entityNumber++;

              //              sim.vars.programGlobalVar__Obsolete_Execution_Model_Halt_Final = false;
              
              
            }
            else{ //no rule matched or selected
              Entity.Trash = true;
              sim.vars.visualization__Last_Executed = "Clock: " + SimSystem.clock() + ". None.";
            }
            
            
            if ( !sim.vars.centralParametersModule.Use_Procedural_Resources ) {
              //ACTR-QN version: 
              if(sim.funs.getNumberOfQnactrEntityInServerAndQueue("matchingandselection") == 0){ //If no more entity in Match and selection, mark server free (not busy).
                sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(  sim.vars.utilization__Production_Module_Changes_In_A_Second,  (double) GlobalUtilities.round(SimSystem.clock() , 3) , 0.0 ) ; 
              }	//utilization = 0 because of single capacity, busy = 1, free = 0.
            }
            else{
              //QN-ACTR version, the capacity of procedural model instanceof   sim.vars.productionModule.Processor_Low_Level_Max_Capacity
              double new_utilization_value = 1.0 - ( sim.vars.productionModule.Processor_Low_Level_Remaining_Capacity / sim.vars.productionModule.Processor_Low_Level_Max_Capacity );
              if (new_utilization_value < 0)System.out.println("Error! Execution ending effect has new_utilization_value < 0");
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Production_Module_Changes_In_A_Second ,  (double)GlobalUtilities.round(SimSystem.clock(),3) , new_utilization_value ) ;
            }
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;

            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case goalbuffer2:
        switch (ServiceStage){
          case Release:
            
            return (!Entity.Trash)&& (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); // (itemsInTask < 1);

          case Beginning:
      
            Entity.Time_Computed = false;
            //currently these do not take time, so they are in the beginning effect
            
            if (Entity.Entity_Type.equals( "First Goal") || Entity.Entity_Type.equals( "Goal Buffer-2's New Chunk")) {
              //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
              int j = 0;
              String old_chunk_name = Entity.Chunk.Chunk_Name;
              String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                j++;
                new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              }
              Entity.Chunk.Chunk_Name = new_chunk_name;
              if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                Entity.Chunk.Activation = (double) 0.0; //all default values
                Entity.Chunk.Creation_Time = 0.0;
                Entity.Chunk.Number_Of_Presentations = 0;
                Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
              }
              sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
              sim.vars.goalBuffer.Goal_Buffer_Chunk_2 = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              
              if(Entity.Entity_Type.equals( "First Goal")){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "SET-BUFFER-CHUNK GOAL-2 "+ Entity.Chunk.Chunk_Name +" REQUESTED NIL"); 
              }
              else if (Entity.Entity_Type.equals( "Goal Buffer-2's New Chunk")) {
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "SET-BUFFER-CHUNK GOAL-2 " + Entity.Chunk.Chunk_Name );  //sim.vars.goalBuffer.Goal_Buffer_Chunk_2.Chunk_Type); 
              }
              else System.out.println("Goal Buffer-2 beginning effect has undefined case: Entity.Entity_Type == " + Entity.Entity_Type);
              
              sim.funs.ProductionModuleFun__Goal_Thread_Add_Or_Update(Entity.Chunk.Chunk_Name);
            }
            
            if (Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice")) {
              
              String[] The_Chunk_Spec_Change = sim.funs.ProgramUtilitiesFun__LinkedListString_To_ArrayString(Entity.The_Chunk_Spec_Change_List);
              Chunk temp_chunk = sim.funs.ChunkFun__Chunk_Clone (sim.vars.goalBuffer.Goal_Buffer_Chunk_2);
              //may add if conditions for chunk name and chunk type change, below is only for slot value change
              int j;
              for (j = 0; j < The_Chunk_Spec_Change.length; j+=2){
                sim.funs.ChunkFun__Set_Chunk_Slot_Value(temp_chunk, The_Chunk_Spec_Change[j], The_Chunk_Spec_Change[j+1]);
              }
              Entity.Chunk = temp_chunk;
              //add the above, moved fromModify_Goal_Buffer_2_Request
              
              sim.vars.goalBuffer.Goal_Buffer_Chunk_2 = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
             sim.funs.ProductionModuleFun__Goal_Thread_Add_Or_Update(Entity.Chunk.Chunk_Name);
              //also modify the chunk in the model chunk list.
              String chunk_name = sim.vars.goalBuffer.Goal_Buffer_Chunk_2.Chunk_Name;
              if (sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false ) System.out.println("Goal Buffer-2 Buffer Chunk Spec Change Notice sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false");
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.centralParametersModule.Chunks, chunk_name, sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk)  );
              if (sim.vars.messageOn) System.out.println ("Chunk Spec Changed at Goal_Buffer_2" );
            }
            
            if (Entity.Entity_Type.equals( "Clear Goal")) {
              if (!sim.vars.goalBuffer.Goal_Buffer_Chunk_2.Chunk_Type.equals("") && !sim.vars.goalBuffer.Goal_Buffer_Chunk_2.Chunk_Name.equals("") ){ //if  buffer is not empty
                sim.funs.ProductionModuleFun__Goal_Thread_Remove(sim.vars.goalBuffer.Goal_Buffer_Chunk_2.Chunk_Name);
                sim.funs.DeclarativeModuleFun__Merge_Chunk_Into_DM (sim.vars.goalBuffer.Goal_Buffer_Chunk_2, "Goal_Buffer_2");
              }
              sim.vars.goalBuffer.Goal_Buffer_Chunk_2 = new Chunk();
            }
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Goal_Buffer_2_Contents ();            
            break;
            
          case Ending:
  
            Entity.From = "Goal Buffer-2"; //change the entity and pass it to Matching And Selection as a trigger
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;

            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
      
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case goalbuffer:
        switch (ServiceStage){
          case Release:
        
            return (!Entity.Trash)&& (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
      
          case Beginning:
   
            Entity.Time_Computed = false;
            //currently these do not take time, so they are in the beginning effect
            
            if (Entity.Entity_Type.equals( "First Goal") || Entity.Entity_Type.equals( "Goal Buffer's New Chunk") ){
              //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
              int j = 0;
              String old_chunk_name = Entity.Chunk.Chunk_Name;
              String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                j++;
                new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              }
              Entity.Chunk.Chunk_Name = new_chunk_name;
              if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                Entity.Chunk.Activation = (double) 0.0; //all default values
                Entity.Chunk.Creation_Time = 0.0;
                Entity.Chunk.Number_Of_Presentations = 0;
                Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
              }
              sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
              sim.vars.goalBuffer.Goal_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              
              if ( Entity.Entity_Type.equals( "First Goal" )) {
                if (sim.vars.messageOn) System.out.println ("First Goal set at sim.vars.goalBuffer. (count) is " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(sim.vars.goalBuffer.Goal_Buffer_Chunk, "count"));
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "SET-BUFFER-CHUNK GOAL "+ Entity.Chunk.Chunk_Name +" REQUESTED NIL"); 
                
              }
              else if (Entity.Entity_Type.equals( "Goal Buffer's New Chunk")) {
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "SET-BUFFER-CHUNK GOAL " + Entity.Chunk.Chunk_Name); 
              }
              else System.out.println("Goal Buffer beginning effect has undefined case: Entity.Entity_Type == " + Entity.Entity_Type);
              
             sim.funs.ProductionModuleFun__Goal_Thread_Add_Or_Update(Entity.Chunk.Chunk_Name);
            }
            
            if (Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice")) {
              
              String[] The_Chunk_Spec_Change = sim.funs.ProgramUtilitiesFun__LinkedListString_To_ArrayString(Entity.The_Chunk_Spec_Change_List);
              Chunk temp_chunk = sim.funs.ChunkFun__Chunk_Clone (sim.vars.goalBuffer.Goal_Buffer_Chunk);
              //may add if conditions for chunk name and chunk type change, below is only for slot value change
              int j;
              for (j = 0; j < The_Chunk_Spec_Change.length; j+=2){
                sim.funs.ChunkFun__Set_Chunk_Slot_Value(temp_chunk, The_Chunk_Spec_Change[j], The_Chunk_Spec_Change[j+1]);
              }
              Entity.Chunk = temp_chunk;
              //add the above, moved fromModify_Goal_Buffer_Request
              
              
              sim.vars.goalBuffer.Goal_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
             sim.funs.ProductionModuleFun__Goal_Thread_Add_Or_Update(Entity.Chunk.Chunk_Name);
              //also modify the chunk in the model chunk list.
              String chunk_name = sim.vars.goalBuffer.Goal_Buffer_Chunk.Chunk_Name;
              if (sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false ) System.out.println("Goal Buffer Buffer Chunk Spec Change Notice sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false");
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.centralParametersModule.Chunks, chunk_name, sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk)  );
              if (sim.vars.messageOn) System.out.println ("Chunk Spec Changed at Goal_Buffer" );
              
              //debug switch
              if(sim.vars.printingModule.Show_Multiple_Goal_Trace_In_Output_Trace_Txt){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("Goal Buffer - Buffer Chunk Spec Change Notice - debug print: " );
                sim.funs.ChunkFun__Print_Chunk(sim.vars.goalBuffer.Goal_Buffer_Chunk);
              }
            }
            
            if (Entity.Entity_Type.equals( "Clear Goal")) {
              if (!sim.vars.goalBuffer.Goal_Buffer_Chunk.Chunk_Type.equals("") && !sim.vars.goalBuffer.Goal_Buffer_Chunk.Chunk_Name.equals("" )){ //if  buffer is not empty
               sim.funs.ProductionModuleFun__Goal_Thread_Remove(sim.vars.goalBuffer.Goal_Buffer_Chunk.Chunk_Name);
                sim.funs.DeclarativeModuleFun__Merge_Chunk_Into_DM (sim.vars.goalBuffer.Goal_Buffer_Chunk, "Goal_Buffer");
              }
              sim.vars.goalBuffer.Goal_Buffer_Chunk = new Chunk();
              //ToDo: add goal buffer state reset
            }
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Goal_Buffer_Contents ();
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            Entity.From = "Goal Buffer"; //change the entity and pass it to Matching And Selection as a trigger
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
            
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;

            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
  
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case imaginalbuffer:
        switch (ServiceStage){
          case Release:
            
            
            //            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
            
          case Beginning:

            //System.out.println(Clock + ", " + Entity.Entity_Type + ": " + sim.funs.ProgramUtilitiesFun__LinkedListString_To_String_Show_Empty(Entity.The_Chunk_Spec_Change_List) );
            
            Entity.Time_Computed = false;
            if (Entity.Entity_Type.equals( "Imaginal Buffer's New Chunk")) {
              //ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(), 3) + "\t" + "IMAGINAL" + "\t" + "SET-BUFFER-CHUNK IMAGINAL " + sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk.Chunk_Type); 
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "IMAGINAL" + "\t" + "SET-BUFFER-CHUNK IMAGINAL " + Entity.Chunk.Chunk_Name ); 
              
              //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
              int j = 0;
              String old_chunk_name = Entity.Chunk.Chunk_Name;
              String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                j++;
                new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              }
              Entity.Chunk.Chunk_Name = new_chunk_name;
              if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                Entity.Chunk.Activation = (double) 0.0; //all default values
                Entity.Chunk.Creation_Time = 0.0;
                Entity.Chunk.Number_Of_Presentations = 0;
                Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
              }
              sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );	
              
              sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk = Entity.Chunk;
              sim.vars.imaginalBuffer.Empty = false;
            }
            
            if (Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice")) {
              
              String[] The_Chunk_Spec_Change = sim.funs.ProgramUtilitiesFun__LinkedListString_To_ArrayString(Entity.The_Chunk_Spec_Change_List);
              Chunk temp_chunk = sim.funs.ChunkFun__Chunk_Clone (sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk);
              //may add if conditions for chunk name and chunk type change, below is only for slot value change
              int j;
              for (j = 0; j < The_Chunk_Spec_Change.length; j+=2){
                sim.funs.ChunkFun__Set_Chunk_Slot_Value(temp_chunk, The_Chunk_Spec_Change[j], The_Chunk_Spec_Change[j+1]);
              }
              Entity.Chunk = temp_chunk;
              //add the above, moved fromModify_Goal_Buffer_2_Request
              
              sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              //also modify the chunk in the model chunk list.
              String chunk_name = sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk.Chunk_Name;
              if (sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false ) System.out.println("Imaginal Buffer Buffer Chunk Spec Change Notice sim.funs.ChunkFun__Is_Chunk_Name(chunk_name) ==false");
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.centralParametersModule.Chunks, chunk_name, sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk)  );
              if (sim.vars.messageOn) System.out.println ("Chunk Spec Changed at Imaginal_Buffer" );
            }
            
            if (Entity.Entity_Type.equals( "Clear Imaginal")) {
              if (!sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk.Chunk_Type.equals("") && !sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk.Chunk_Name.equals("")){ //if imaginal buffer is not empty
                sim.funs.DeclarativeModuleFun__Merge_Chunk_Into_DM (sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk, "Imaginal_Buffer");
              }
              sim.vars.imaginalBuffer.Imaginal_Buffer_Chunk = new Chunk();
              sim.vars.imaginalBuffer.Empty = true;
              sim.vars.imaginaryModule.State_Error = false;
              
            }
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Imaginal_Buffer_Contents();
            //System.out.println("IMAGINAL BUFFER: stored a chunk:");
            //sim.funs.ChunkFun__Print_Chunk(Entity.Chunk);
            break;
            
          case Ending:

            Entity.From = "Imaginal Buffer"; //change the entity and pass it to Matching And Selection as a trigger
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
            
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;

            
            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;

          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;	
        }
        break;
        
      case imaginarymodule:
        switch (ServiceStage){
          case Release:
            
            boolean returnBool = (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1) 
            
            //System.out.println("ServerLogics, " + ServerName + ", " + ServiceStage + ". " + Entity.Entity_Type + " returnBool: " + returnBool);
            
            //            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return returnBool;
         
          case Beginning:
            
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            if (Entity.Entity_Type.equals( "Add Imaginal")){
              sim.vars.imaginaryModule.State_Free = false;
              //sim.vars.imaginaryModule.State_Busy = true;
            }
            
//            System.out.println("ServerLogics, " + ServerName + ", " + ServiceStage + ". " + Entity.Entity_Type + " Done");
            
            break;
            
          case Ending:
            boolean debug = false;
            
            if (Entity.Entity_Type.equals( "Add Imaginal")) { 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Imaginary Module"; //tag from and to
              Entity.To = "Imaginal Buffer";
              Entity.Entity_Type = "Imaginal Buffer's New Chunk";    
              //Entity.Chunk just be the same
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(), 3) + "\t" + "IMAGINAL" + "\t" + "CREATE-NEW-BUFFER-CHUNK IMAGINAL ISA "+Entity.Chunk.Chunk_Type); 
              sim.vars.imaginaryModule.State_Free = true;
              //sim.vars.imaginaryModule.State_Busy = false;
            }
            if (Entity.Entity_Type.equals( "Modify Imaginal")) { 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Imaginary Module"; //tag from and to
              Entity.To = "Imaginal Buffer";
              Entity.Entity_Type = "Buffer Chunk Spec Change Notice";    
              //Entity.Chunk just be the same
            }
            if (Entity.Entity_Type.equals( "Clear Imaginal")) { 
              //debug
              //GlobalUtilities.popUpMessage("Imaginary Module clear imaginal");
              
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Imaginary Module"; //tag from and to
              Entity.To = "Imaginal Buffer";
              Entity.Entity_Type = "Clear Imaginal";    
              //Entity.Chunk just be the same
            }
            
            
            sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Imaginary_Module_Changes_In_A_Second, (double)GlobalUtilities.round (SimSystem.clock(),3) , 0.0 );
            
            if(debug)System.out.println("ServerLogics, " + ServerName + ", " + ServiceStage + ". " + Entity.Entity_Type + " Done.");
            
            break;
            
          case Timing:
            boolean debug1 = false;
            double duration;
            if (Entity.Entity_Type.equals( "Add Imaginal")){
              Entity.Time_Computed = true;
              duration = sim.vars.imaginaryModule.Imaginal_Delay;
            }
            else {
              Entity.Time_Computed = true;
              duration = 0.0;
            }
            
            if(duration > 0.0 ) sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Imaginary_Module_Changes_In_A_Second, (double)GlobalUtilities.round (SimSystem.clock(),3) , 1.0 ) ; 
            
            if(debug1)System.out.println("ServerLogics, " + ServerName + ", " + ServiceStage + ". " + Entity.Entity_Type + " Done. duration: " + duration);
            
            return duration;
        }
        break;
        
      case intentionalmodule:
        switch (ServiceStage){
          case Release:
            //            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash)&& (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
       
          case Beginning:
            
            Entity.Time_Computed = false;
            //goal module's 'State free' will always be t.
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            if (Entity.Entity_Type.equals( "Add Goal")) { 
              
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer";
              Entity.Entity_Type = "Goal Buffer's New Chunk";    
              //Entity.Chunk just be the same
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "CREATE-NEW-BUFFER-CHUNK GOAL ISA "+Entity.Chunk.Chunk_Type); 
              
            }
            if (Entity.Entity_Type.equals( "Modify Goal")) { 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer";
              Entity.Entity_Type = "Buffer Chunk Spec Change Notice";    
              //Entity.Chunk just be the same
            }
            if (Entity.Entity_Type.equals( "Clear Goal")) { 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer";
              Entity.Entity_Type = "Clear Goal";    
              //Entity.Chunk just be the same
            }
            if (Entity.Entity_Type.equals( "Add Goal-2") ){ 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer-2";
              Entity.Entity_Type = "Goal Buffer-2's New Chunk";    
              //Entity.Chunk just be the same
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "GOAL" + "\t\t" + "CREATE-NEW-BUFFER-CHUNK GOAL-2 ISA "+Entity.Chunk.Chunk_Type); 
            }
            if (Entity.Entity_Type.equals( "Modify Goal-2")) { 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer-2";
              Entity.Entity_Type = "Buffer Chunk Spec Change Notice";    
              //Entity.Chunk just be the same
            }
            if (Entity.Entity_Type.equals( "Clear Goal-2") ){ 
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Intentional Module"; //tag from and to
              Entity.To = "Goal Buffer-2";
              Entity.Entity_Type = "Clear Goal";    
              //Entity.Chunk just be the same
            }
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;

        }
        break;
        
      case manualbuffer:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;

            
          case Beginning:
            Entity.Time_Computed = false;
            Entity.Trash = false;
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Motor_Details();
            
            break;
            
          case Ending:

            Entity.Chunk = null;
            Entity.Response_Item = null;
            Entity.From = "Manual Buffer";
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;

            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;

        }
        break;
        
      case matchingandselection:
        switch (ServiceStage){
          case Release:
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            if (sim.vars.centralParametersModule.Use_Procedural_Resources) {
              return (!Entity.Trash ); //may take multiple entities in QN
            }
            else {
              return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            }

          case Beginning:

            //GlobalUtilities.popUpMessage("test");
            
            Entity.Event_Priority = 0; //reset this value that may be modified in triggerbuffer
            
            Entity.Time_Computed = false;
            
            int round_number = 1;
            LinkedList<Production_Rule> rule_pool_to_be_matched = new LinkedList<Production_Rule>();
            LinkedList<Production_Rule> selected_rules_list = new LinkedList<Production_Rule>();
            
            while( true ) {  //inside the while loop will have exits to EXIT THE LOOP WHEN NO RULE TO MATCH
              //determine the pool of rules to be matched.
              if( round_number == 1 ){ //first round matching using all rules.
                rule_pool_to_be_matched = sim.vars.productionModule.Production_Rules;
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "PROCEDURAL" + "\t" + "CONFLICT-RESOLUTION"); 
              }
              else {
                // rule_pool_to_be_matched = Entity.Rule_Pool;  // NEED TO UPDATE THE RULE POOL AFTER EACH ROUND OF MATCH AND SELECTION
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "PROCEDURAL" + "\t" + "CONFLICT-RESOLUTION QN: Same cycle multiple matches"); 
              }
              
              
              
              
              //new method Match Rule
              
              //match rule pointer version cannot handle =goal-x> in condition
              //LinkedList<Production_Rule> matched_rules_pointer =Match_Rule(); //this is tested to be pointer rather than clone
              
              
        			//debug switch showing goal buffer
        			//ProgramUtilitiesFun__Output_Trace_Txt("");
        			//ProgramUtilitiesFun__Output_Trace_Txt("~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~~~~~");
        			
        			//ProgramUtilitiesFun__Output_Trace_Txt("Matching And Selection debug print Goal Buffer and Goal-2 buffer: " );
        			//sim.funs.ChunkFun__Print_Chunk(sim.vars.goalBuffer.Goal_Buffer_Chunk);
        			//ProgramUtilitiesFun__Output_Trace_Txt("--------------");
        			//sim.funs.ChunkFun__Print_Chunk(sim.vars.goalBuffer.Goal_Buffer_Chunk_2);
        			//ProgramUtilitiesFun__Output_Trace_Txt("~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~DEBUG~~~~~~~~~~~~~~~~~~~~~~");
        			//ProgramUtilitiesFun__Output_Trace_Txt("");
               
              
              //match rule clone can handle =goal-x> in condition
              LinkedList<Production_Rule> matched_rules_clone = sim.funs.ProductionModuleFun__Match_Rule(rule_pool_to_be_matched);
              
              //debug showing the matched rule(s)
              //System.out.println("debug showing the matched rule(s)");
              //for(Production_Rule rule:matched_rules_clone) System.out.println("Matched rule: "+  rule.Rule_Name);
        			//IEnumerator enum_rules = matched_rules_clone.GetEnumerator();
        			//while(enum_rules.MoveNext()){
        			//	GlobalUtilities.popUpMessage("Matched rule: " +  ((Production_Rule)enum_rules.Current).Rule_Name );
        			//}
              
              if (matched_rules_clone == null || matched_rules_clone.size() == 0 ){ // no match
                if ( round_number == 1 ) { // this is the first round, go through the normal selection path, because select rule can handle no match condition.
                  sim.vars.visualization__Last_Match = "Clock: " + SimSystem.clock() + ". No match."; //only change this for the first round
                }
                else{ // this is 2nd round or later, no match, so kill this entity, no selection needed. NEED TO CHANGE THIS TO SOMETHING ELSE
                  if (sim.vars.centralParametersModule.Use_Procedural_Resources == false) { //double check
                    System.out.println( "Error! Matching And Selection has 2nd round or later, no match, when Use_Procedural_Resources == false");
                    break; 
                  }
                  //debug
                  //ProgramUtilitiesFun__Output_Trace_Txt("________Match and select rule: no match at round: " + round_number);
                  
                }
                
                //do these that are otherwise done in selection function.
                if(sim.vars.printingModule.v.equals( "t")) sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("________Match and select rule: End________");
                if(sim.vars.printingModule.v.equals( "t")) sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("");
                
                break; //break the while loop, exit match and selection. rules list will have .count == 0.
                
              }
              else { //there is any match. selection needed. 
                
                itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
                if(itemsInTask > 1) sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("** multiple rules processing within a procedural cycle **"); 
                
                if ( round_number == 1 ) { // this is the first round
                  //update Last Match visualization only in the first round of match.
                  String all_rules = "";
                  for(Production_Rule a_rule : matched_rules_clone){
                    all_rules += a_rule.Rule_Name + "  ";
                  }
                  sim.vars.visualization__Last_Match = "Clock: " + SimSystem.clock() + ". " + all_rules  ;
                }
                else if ( round_number > 1 ){ // this is 2nd round or later
                  if (sim.vars.centralParametersModule.Use_Procedural_Resources == false) { //double check
                    System.out.println( "Error! Matching And Selection has 2nd round or later, matched, when Use_Procedural_Resources == false");
                    break; 
                  }
                }
                else { //Halt program.
                  System.out.println( "Error! Matching And Selection has undefined round_number: " + round_number);
                  break;
                }
                //see selection part below
              }
              
              if (!Entity.Trash){ // selection.
                //new method Select Rule
                Production_Rule the_selected_rule_clone = sim.funs.ProductionModuleFun__Select_Rule(matched_rules_clone);
                
                if (!the_selected_rule_clone.Rule_Name.equals( "nil" )){ //when there is any match, and of course a selection.
                  selected_rules_list.addLast(the_selected_rule_clone);
                  sim.vars.programGlobalVar__ProductionModule_Currently_Processing_Rules_Name_List.addLast( the_selected_rule_clone.Rule_Name);
                  
                  if (sim.vars.centralParametersModule.Use_Procedural_Resources) {
                    //reduce processor capacities
                    sim.vars.productionModule.Processor_Low_Level_Remaining_Capacity -= the_selected_rule_clone.Num_Low_Level_Requests;
                    sim.vars.productionModule.Processor_High_Level_Remaining_Capacity -= the_selected_rule_clone.Num_High_Level_Requests;
                    sim.vars.productionModule.Processor_Aural_Action_Remaining_Number -= the_selected_rule_clone.Num_Aural_Action;
                    sim.vars.productionModule.Processor_Aural_Location_Action_Remaining_Number  -= the_selected_rule_clone.Num_Aural_Location_Action ;
                    sim.vars.productionModule.Processor_Goal_Action_Remaining_Number  -= the_selected_rule_clone.Num_Goal_Action ;
                    sim.vars.productionModule.Processor_Imaginal_Action_Remaining_Number  -= the_selected_rule_clone.Num_Imaginal_Action ;
                    sim.vars.productionModule.Processor_Manual_Action_Remaining_Number  -= the_selected_rule_clone.Num_Manual_Action ;
                    sim.vars.productionModule.Processor_Retrieval_Action_Remaining_Number  -= the_selected_rule_clone.Num_Retrieval_Action ;
                    sim.vars.productionModule.Processor_Visual_Action_Remaining_Number  -= the_selected_rule_clone.Num_Visual_Action ;
                    sim.vars.productionModule.Processor_Visual_Location_Action_Remaining_Number  -= the_selected_rule_clone.Num_Visual_Location_Action ;
                    sim.vars.productionModule.Processor_Vocal_Action_Remaining_Number  -= the_selected_rule_clone.Num_Vocal_Action ;
                    
                    
                    //compute a new rule pool to be matched (remove the matched rule and its homogeneous, compiled_from rules, from the list)
                    //remaining_rule_pool_to_be_matched (Temp_Entity.Rule_Pool) = Clone of matched_rules_clone - same family name rules of the_selected_rule_clone (removing rule names).
                    LinkedList<Production_Rule> remaining_rule_pool_to_be_matched = new LinkedList<Production_Rule> ();
                    List<String> removing_rule_names = new ArrayList<String> ();
                    
                    removing_rule_names.add(the_selected_rule_clone.Rule_Name); //add the selected rule
                    
                    //removing_rule_names = sim.funs.ProgramUtilitiesFun__Add_Sons_And_Sons_Of_A_Father_Rule_Into_ListString ( the_selected_rule_clone.Rule_Name,  removing_rule_names);
                    //removing_rule_names = sim.funs.ProgramUtilitiesFun__Add_Fathers_And_Fathers_Of_A_Rule_Into_ListString  ( the_selected_rule_clone.Rule_Name,  removing_rule_names);
                    String the_oldest_grandfather_name = sim.funs.ProgramUtilitiesFun__Get_Compilation_Oldest_Grandfather_Name(the_selected_rule_clone.Rule_Name);
                    if(!removing_rule_names.contains(the_oldest_grandfather_name)) removing_rule_names.add(the_oldest_grandfather_name);
                    removing_rule_names = sim.funs.ProgramUtilitiesFun__Add_Sons_And_Sons_Of_A_Father_Rule_Into_ListString(the_oldest_grandfather_name , removing_rule_names);
                    
                    for (Production_Rule a_rule : matched_rules_clone){
                      if (removing_rule_names.contains ( a_rule.Rule_Name ) == false ){
                        remaining_rule_pool_to_be_matched.addLast ( sim.funs.ProgramUtilitiesFun__Production_Rule_Clone( a_rule ) );	
                      }
                    }
                    
                    //debug
                    //if(Clock >= 205.27)GlobalUtilities.popUpMessage("the_selected_rule_clone.Rule_Name: " + the_selected_rule_clone.Rule_Name + "\nthe_oldest_grandfather_name: " + the_oldest_grandfather_name + "\nremoving_rule_names: " + sim.funs.ProgramUtilitiesFun__ListString_To_String_Show_Empty(removing_rule_names) + "\nremaining_rule_pool_to_be_matched.Count: " + remaining_rule_pool_to_be_matched.Count );
                    
                    if(remaining_rule_pool_to_be_matched.size() > 0 ){
                      rule_pool_to_be_matched = remaining_rule_pool_to_be_matched;
                    }
                    else {
                      //Debug
                      //ProgramUtilitiesFun__Output_Trace_Txt("________Match and select rule: remaining_rule_pool_to_be_matched.Count: " + remaining_rule_pool_to_be_matched.Count);
                      break; //break the while loop, exit match and selection.
                    }
                  }	
                }
                else { // when no match,.equals( "nil"), do not reduce capacity; do not create another entity; just double check and go through the rest process
                  if (matched_rules_clone.size() != 0) {
                    System.out.println("Error! Matching and selection has no match double check error! ");
                    break;
                  }
                }
                
                if(sim.vars.printingModule.Show_Multiple_Goal_Trace_In_Output_Trace_Txt) sim.funs.ProgramUtilitiesFun__Output_Trace_Txt( " matching and selection: sim.vars.productionModule.Goal_Thread_Order_List_For_Threaded_Cognition: " + sim.funs.ProgramUtilitiesFun__StringArray_To_String_Show_Empty(sim.funs.ProgramUtilitiesFun__LinkedListString_To_ArrayString(sim.vars.productionModule.Goal_Thread_Order_List_For_Threaded_Cognition) ));
                
                
                
                //debug tests
                //GlobalUtilities.popUpMessage( GlobalUtilities.round(SimSystem.clock(), 3) + "    selected rule: " +Select_Rule(matched_rules_pointer).Rule_Name );
                
                
                //old method
                //Entity.Entity_Type =Match_And_Select_Rule_Obsolete_Plus_Output_Trace (Match_And_Select_Rule_Old() );
                
                
                //debug
                //if (GlobalUtilities.round (SimSystem.clock(), 3) > 50 ) System.out.println ("Matching and selection: " + Entity.Entity_Type + "VL buffer chunk : " + sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Type);
                
              }
              
              if ( !sim.vars.centralParametersModule.Use_Procedural_Resources) break; // only one round for ACT-R.
              
              round_number++;
            } //end of match and selection loop
            
            
            //sending an entity to execution.
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber;
            //QnactrSimulation.entityNumber++;
            Entity.From = "Matching And Selection";
            Entity.To = "Execution";
            
            //Entity.Entity_Type = the_selected_rule_clone.Rule_Name; //now there may be multiple rules selected
            Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution = selected_rules_list;
            // for each rule, store the variable binding at the time of selection (now)
            for(Production_Rule rule : selected_rules_list) {
              rule.Variable_Binding = sim.funs.ProductionModuleFun__Bind_Variables_In_Rule_Condition(rule);
            }
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            // no ending effect
            
            break;
            
          case Timing:

            //String the_selected_rule_name = Entity.Entity_Type; //OLD
            
            double duration;
            //if (!the_selected_rule_name.equals( "nil")) {  //OLD if something selected.
            
            if (Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.size() == 1) {  //if something selected.
              Production_Rule the_rule_to_fire = Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.getFirst();
              if (sim.vars.utilityModule.utility_Computation_Method.equals( "PG-C")) {
                duration = Double.parseDouble( (String)sim.vars.utilityModule.PG_C_effort_rule_firing_time.get(the_rule_to_fire));
              }
              else {
                duration = the_rule_to_fire.Action_Time;
              }
            }
            else if (Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.size() > 1){
              System.out.println("TODO in Matching and selection. Entity.Production_Rules_List_Clone_From_Matching_And_Selection_To_Execution.Count > 1");
              duration = 0.050; //temp
            }
            //else if (sim.vars.programGlobalVar__Obsolete_Execution_Model_Halt_Final == true) return 0.000000000000001; //give extra time and make sure there is no incoming trigger ToDo: does this work well?
            else duration = 0.0;
            
            
            
            if ( duration > 0.0) { //utilization end in Execution task ending effect
              //GlobalUtilities.popUpMessage("the_selected_rule_name: " + the_selected_rule_name);
              
              double new_utilization_value;
              if ( !sim.vars.centralParametersModule.Use_Procedural_Resources ) {
                //ACTR-QN version: 
                new_utilization_value = 1.0; //utilization = 1 because of single capacity, busy = 1, free = 0.
              }
              else{
                //QN-ACTR version, the capacity of procedural model is   sim.vars.productionModule.Processor_Low_Level_Max_Capacity
                new_utilization_value = 1.0 - ( sim.vars.productionModule.Processor_Low_Level_Remaining_Capacity / sim.vars.productionModule.Processor_Low_Level_Max_Capacity );
              }
              sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Production_Module_Changes_In_A_Second ,  (double)GlobalUtilities.round(SimSystem.clock(),3) , new_utilization_value ) ;
              
            }
            Entity.Time_Computed = true;
            return duration;

        }
        break;
        
      case motorexecution:
        switch (ServiceStage){
          case Release:
            boolean is_resource_enough = true;
            //check each type of resource. if any resource is not enough, it is not enough
            if( sim.vars.motorModule.Execution_Resource_Left_Hand < sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Left_Hand") ) is_resource_enough = false;
            else if ( sim.vars.motorModule.Execution_Resource_Right_Hand < sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Right_Hand") ) is_resource_enough = false;
            //may add more
            
            
            //System.out.println(Clock + " " + request_type + " " + is_resource_enough  + " " + sim.vars.motorModule.Execution_Resource_Left_Hand + " " + sim.vars.motorModule.Execution_Resource_Right_Hand);
            //return (!Entity.Trash ) && (itemsInTask < 1) ; //single motor resource version
            // return (!Entity.Trash )	&& is_resource_enough; //multiple motor resources version (body parts) // checked at motor initiation
            return (!Entity.Trash );

            
          case Beginning:
            
            
            Entity.Time_Computed = false;
            //1. beginning effect; 2. timing; 3. ending effect
            sim.vars.motorModule.Execution_Free = false;
            
            if (Entity.From.equals( "Motor Initiation") && Entity.To.equals( "Motor Execution") && Entity.Entity_Type.equals( "Add Manual")){ //copy an entity to control for motor execution results
              if(!Entity.Chunk.Chunk_Type.equals( "clear")){
        
                Entity Temp_Entity = sim.funs.createEntity( "Control Motor" , "Motor Execution", "Control Motor", "Motor Execution Result", 0.0);
                //                Entity Temp_Entity = new Entity();  
                //                Temp_Entity.ID = "221"; //Control Motor
                //                Temp_Entity.Time = (double) SimSystem.clock();
                //                Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
                //                QnactrSimulation.entityNumber++;
                //                Temp_Entity.From = "Motor Execution"; //tag from and to
                //                Temp_Entity.To = "Control Motor";
                //                Temp_Entity.Entity_Type = "Motor Execution Result";   
        
                Temp_Entity.Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
                Temp_Entity.Note = Entity.Note;
                Entity.Note = "";
                // if this is a move-cursor with incremental moves, take out the incremental moves stuff because those have already been sent to control motor
                if(Temp_Entity.Chunk.Chunk_Type.equals( "move-cursor") && Temp_Entity.Chunk.Slot.containsKey("incremental-movements")) {
                  // remove incremental movement object from this one
                  Temp_Entity.Chunk.Slot.remove("incremental-movements");
                }

                
              }
              
              //compute resource taking.
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                case "press-key":
                {
                  
                  String hand = sim.funs.DeviceModuleFun__Get_Press_Key_Motor_Command_Attribute(sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key"), "hand");
                  
                  if(hand.equals( "left")) {
                    sim.vars.motorModule.Execution_Resource_Left_Hand --;
                  }
                  else if(hand.equals( "right")){
                    sim.vars.motorModule.Execution_Resource_Right_Hand --;
                    
                  }
                  else System.out.println("WARNING! Motor Execution has undefined hand: " + hand);
                  
                  break;
                }
                case "punch":
                case "tap":
                case "click-mouse":
                case "move-cursor":
                case "move-hand-touch":
                case "world3d-driving-two-point-visual-manual-steer":	
                case "world3d-sp-driving-control-manual-joystick":
                case "customized-manual-action":
                case "clear":
                { 
                  //ToDo
                  break;
                }
                
                case "":{
                  System.out.println("Motor Execution beginning effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Motor Execution beginning effect has undefine case: " + request_type);
                  break;
                }	
              }//end of resource taking
              
            }
            
            break;
            
          case Ending:

            //here the movement is finished, fingers return to rest position
            if (Entity.From.equals( "Motor Initiation") && Entity.To.equals( "Motor Execution") && Entity.Entity_Type.equals( "Add Manual")){
              String request_type = Entity.Chunk.Chunk_Type;
              String details = "";
              if( request_type.equals( "press-key")) details += "press-key " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
              else details += request_type;
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "FINISH-MOVEMENT " + details); 
              
              
              //compute resource giving back.
              switch(request_type){
                case "press-key":
                {
                  
                  String hand = sim.funs.DeviceModuleFun__Get_Press_Key_Motor_Command_Attribute(sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key"), "hand");
                  //System.out.println(hand);			
                  if(hand.equals( "left")) {
                    sim.vars.motorModule.Execution_Resource_Left_Hand ++;
                  }
                  else if(hand.equals( "right")){
                    sim.vars.motorModule.Execution_Resource_Right_Hand ++;
                  }
                  else System.out.println("WARNING! Motor Execution Ending effect has undefined hand: " + hand);
                  
                  break;
                }
                case "punch":
                case "tap":
                case "click-mouse":
                case "move-cursor":
                case "move-hand-touch":
                case "world3d-driving-two-point-visual-manual-steer":	
                case "customized-manual-action":
                case "world3d-sp-driving-control-manual-joystick":
                case "clear":
                { 
                  //ToDo
                  break;
                }
                
                case "":{
                  System.out.println("Motor Execution Ending effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Motor Execution Ending effect has undefine case: " + request_type);
                  break;
                }	
              }//end of resource giving back
              
              
            }
            else System.out.println( "Motor Execution ending effect has undefined case");
            
            //prepare visualization of manual response, back to rest position
            if(sim.vars.animatorModule.Show_Animator){
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                case "press-key":{ 
                  String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key"); //this is the key that is pressed
                  Chunk command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.get(key);
                  String hand_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "hand");
                  String finger_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "finger");
                  Two_Tuple finger_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand_string, finger_string);
                  String rest_on_key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key ( (int) finger_location.Ob1, (int) finger_location.Ob2);
                  sim.funs.AnimatorModuleFun__Set_Finger_Rest_On_A_Place( hand_string, finger_string, rest_on_key ); 
                  
                  break;
                }
                
                case "punch": { //Entity.Chunk is the command
                  String hand_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                  String finger_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finger");
                  Two_Tuple finger_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand_string, finger_string);
                  String rest_on_key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key ( (int) finger_location.Ob1, (int) finger_location.Ob2);
                  sim.funs.AnimatorModuleFun__Set_Finger_Rest_On_A_Place( hand_string, finger_string, rest_on_key ); 
                  
                  break;
                }
                
                case "click-mouse": { //Clicking the mouse is really just a punch with the right index finger. ACT-R 6.0 motor.lisp
                  String hand_string = "right";
                  String finger_string = "index";
                  Two_Tuple finger_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand_string, finger_string);
                  String rest_on_key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key ( (int) finger_location.Ob1, (int) finger_location.Ob2);
                  sim.funs.AnimatorModuleFun__Set_Finger_Rest_On_A_Place( hand_string, finger_string, rest_on_key ); 
                  
                  break;
                }		
                case "tap":
                case "move-cursor":
                case "move-hand-touch":
                case "world3d-driving-two-point-visual-manual-steer":
                case "customized-manual-action":
                case "world3d-sp-driving-control-manual-joystick":	
                case "clear":
                {
                  //nothing to do
                  break;
                }
                case "":{
                  System.out.println("Motor Execution ending effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Motor Execution ending effect has undefine case: " + request_type);
                  break;
                }	
              }
            }
            int itemsInTask24 = sim.funs.getNumberOfQnactrEntityInServer("motorexecution");
            int itemsInQueue24 = sim.funs.getNumberOfQnactrEntityInQueue("motorexecution");
            int itemsInTask23 = sim.funs.getNumberOfQnactrEntityInServer("motorinitiation");
            int itemsInQueue23 = sim.funs.getNumberOfQnactrEntityInQueue("motorinitiation");
            
            if(itemsInQueue24 == 0 && itemsInTask24 == 1 && itemsInQueue23 == 0 && itemsInTask23 == 0) sim.vars.motorModule.Execution_Free = true;
            
            if(sim.vars.motorModule.Preparation_Free && sim.vars.motorModule.Processor_Free && sim.vars.motorModule.Execution_Free ) sim.vars.motorModule.State_Free = true;
            Entity.From = "Motor Execution"; //tag from and to
            Entity.To = "";
            
            
            if(sim.vars.motorModule.State_Free) sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.utilization__Motor_Module_Changes_In_A_Second, (double)GlobalUtilities.round(SimSystem.clock(),3) , 0.0 );
            
            
            Entity.Entity_Type = "Motor Finish";
            
            
            //send a JMT ACK event to motor initiation queue section to activate any queued entities due to limited motor resources
            sim.funs.sendAckToQueue("Motor Initiation", 0.0); // this is needed for the is_resource_enough test in motor initiation release condition to work.
            
            
            break;
            
          case Timing:
            //timing here is for finishing movement, which is the time hand back to designated place for the press-key case. This is not the time that the control system gets the output. That one is computed by execution time. 
            double motor_finish_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_finish_time" ) );
            Entity.Time_Computed = true;
            return motor_finish_time;

        }
        break;
        
      case motorinitiation:
        switch (ServiceStage){
          case Release:

            //check motor channel (body parts) resources, get sum of resource for all entities in Motor Execution ID = 24, check each type of resource. 
            int resource_left_hand_being_used = 0, resource_right_hand_being_used = 0, resource_left_foot_being_used = 0, resource_right_foot_being_used = 0;
            
            Iterator<Entity> itr_Entity = sim.funs.getLinkedListOfQnactrEntityInServerAndQueue("motorexecution").iterator(); 
            while(itr_Entity.hasNext()){
              Entity an_entity = itr_Entity.next();

              resource_left_hand_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Left_Hand");
              resource_right_hand_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Right_Hand");
              resource_left_foot_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Left_Foot");
              resource_right_foot_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Right_Foot");
            }
            
            
            //fix bug, 2015-01-15
            //old//itr_Entity = sim.funs.getLinkedListOfQnactrEntityInQueue("motorinitiation").iterator(); //            for(Entity an_entity : Model.Find ( e => (e.ID == "23") && !(e.Event.ToString().equals( "EvaluateQueue"))  )){
            //new
            itr_Entity = sim.funs.getLinkedListOfQnactrEntityInServer("motorinitiation").iterator(); //            for(Entity an_entity : Model.Find ( e => (e.ID == "23") && !(e.Event.ToString().equals( "EvaluateQueue"))  )){
            
            while(itr_Entity.hasNext()){
              Entity an_entity = itr_Entity.next();

              resource_left_hand_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Left_Hand");
              resource_right_hand_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Right_Hand");	
              resource_left_foot_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Left_Foot");
              resource_right_foot_being_used += sim.funs.MotorModuleFun__Get_Request_Resource_Needed(an_entity.Chunk, "Execution_Resource_Right_Foot");
            }
            
            
            int resource_left_hand_this_entity_need = sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Left_Hand");
            int resource_right_hand_this_entity_need = sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Right_Hand");	
            int resource_left_foot_this_entity_need = sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Left_Foot");
            int resource_right_foot_this_entity_need = sim.funs.MotorModuleFun__Get_Request_Resource_Needed(Entity.Chunk, "Execution_Resource_Right_Foot");	
            boolean is_resource_enough = true;
            if( resource_left_hand_being_used + resource_left_hand_this_entity_need > 1 ) is_resource_enough = false;
            else if ( resource_right_hand_being_used + resource_right_hand_this_entity_need > 1 ) is_resource_enough = false;
            else if ( resource_left_foot_being_used + resource_left_foot_this_entity_need > 1 ) is_resource_enough = false;
            else if ( resource_right_foot_being_used + resource_right_foot_this_entity_need > 1 ) is_resource_enough = false;
            //may add more
            
            
            //if motor output sequence required, check whether the last motor action output is finished (control motor got the output, but motor execution may not be over considering its cool down time).
            boolean is_motor_output_sequence_ok;
            if (!sim.vars.motorModule.Motor_Output_Sequence_Required) is_motor_output_sequence_ok = true;
            
            else if (resource_left_foot_this_entity_need > 0 || resource_right_foot_this_entity_need > 0 ) is_motor_output_sequence_ok = true; // temp setting for manual + pedal dual task.
            
            else {
              //System.out.println(sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request + " " + sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request.equals( "" ));
              if(sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request.equals( "" )) is_motor_output_sequence_ok = true;
              else is_motor_output_sequence_ok = false;
            }
            
            //System.out.println(Model.Find("ID", "23").Count + " " + Model.FindTask("23").ItemsInQueue);
            //System.out.println(Model.Find("ID", "24").Count + " " + Model.FindTask("24").ItemsInQueue);
            //if(Entity.Chunk.Chunk_Type.equals( "customized-manual-action")){
            //        			System.out.println(Clock + " " + resource_right_foot_being_used + " " + resource_right_foot_this_entity_need + " " + is_resource_enough + " " + is_motor_output_sequence_ok);
            //}
            //return (!Entity.Trash ) && (itemsInTask < 1);
            //return (!Entity.Trash ) && (itemsInTask < 1) && sim.vars.motorModule.Execution_Free; 
            //return (!Entity.Trash )  && is_resource_enough && is_motor_output_sequence_ok;
            
                        
            
            
            
            //return is_motor_output_sequence_ok;       // after motor key output, immediately motor initiation next key,resulting in shorter inter-key time
            //return is_resource_enough;  //after motor finish, initiate next key,resulting in longer inter-key time         
            
            if(is_motor_output_sequence_ok && is_resource_enough){  //moved to here from the beginning effect, because it is too late there. sometimes two entities will go together through this release condition, and then enter the beginning effect together, causing errors.
              
              if(Entity.Chunk.Chunk_Type.equals( "customized-manual-action")){
                //do not check is_motor_output_sequence_ok for these.
                //GlobalUtilities.popUpMessage("do not check is_motor_output_sequence_ok for these");
              }
              else if (sim.vars.motorModule.Motor_Output_Sequence_Required) sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request = sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk); 
	    
	    }
            
            return is_motor_output_sequence_ok && is_resource_enough; //when resources are defined for each motor command and is_resource_enough is used, using both equal to using just  is_resource_enough; but otherwise, is_motor_output_sequence_ok may be used alone. 
            
            
            
            //for typing task, actually, if only applying is_motor_output_sequence_ok test, initiation of next key will only start when the previous key pressing has issued Output (but has not finished return to original position). 
            // if only applying is_resource_enough, initiation of next key will only start when the previous key typing has finished and returned to the original position, resulting in longer inter-key time than only applying is_motor_output_sequence_ok test
            //when both is used, it is equal to using only is_resource_enough.
            
            
        		// imperfect error prevention waiting
        		//if (resource_left_foot_this_entity_need > 0 || resource_right_foot_this_entity_need > 0 ){ // this is a pedal entity
        		//	return is_resource_enough;  
        		//}
        		//else{	//hand entity
        		//	if( !is_resource_enough ) { //same hand , that is, same hand 2 fingers and 1 finger cases in Wu and Liu 2008.
        		//		double error_prevention_duration = Math.Max(0.0, Distributions.Normal( 0.108 , 0.04 ));
        		//		sim.funs.ChunkFun__Set_Chunk_Slot_Value( Entity.Chunk, "motor_initiation_time" , (sim.vars.motorModule.Motor_Initiation_Time + error_prevention_duration).ToString() );
        		//		return (itemsInTask < 1);
        		//	}
        		//	else { // alter hand
        		//		double error_prevention_duration = Math.Max(0.0, Distributions.Normal( 0.108 , 0.04 ));
        		//		sim.funs.ChunkFun__Set_Chunk_Slot_Value( Entity.Chunk, "motor_initiation_time" , (sim.vars.motorModule.Motor_Initiation_Time + error_prevention_duration).ToString() );
        		//		return (true);
        		//	}
        		//}
             
            
            
            //adding is_resource_enough, listening to Execution status is similar to QN-MHP's server X: feedback information collection
            
          case Beginning:
            
            if(!Entity.Trash ) {
              
              //Entity.Time = 0.0;
              Entity.Time_Computed = false;
              //1. beginning effect; 2. timing; 3. ending effect
              sim.vars.motorModule.Processor_Free = false;
              sim.vars.motorModule.Execution_Free = false;
              
              
              //System.out.println(Clock + " Initiated: " + sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request );
              
              // if this is a move-cursor and incremental moves are enabled, send an entity to control motor now to start the intermediate movements
              if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && Entity.Chunk.Slot.containsKey("incremental-movements")) {
                // create a new entity
                Entity Move_Entity = sim.funs.createEntity( "Control Motor" , "Motor Execution", "Control Motor", "Motor Execution Result", 0.0);
                //                Entity Move_Entity = new Entity();
                //                Move_Entity.ID = "221"; // Control Motor
                //                Move_Entity.Time = (double) SimSystem.clock();
                //                Move_Entity.Tag = QnactrSimulation.entityNumber;
                //                QnactrSimulation.entityNumber++;
                //                // claim this is from motor execution because control motor checks for it
                //                // TODO i guess we should change control motor to check for motor initiation
                //                Move_Entity.From = "Motor Execution";
                //                Move_Entity.To = "Control Motor";
                //                Move_Entity.Entity_Type = "Motor Execution Result";
                Move_Entity.Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
                Move_Entity.Note = Entity.Note;
                // add init time to execution time because these will start right now
                double init_time = 0;
                if(Move_Entity.Chunk.Slot.containsKey("motor_initiation_time") ) { //use customized time
                  init_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_initiation_time" ) );
                } else {
                  init_time = sim.vars.motorModule.Motor_Initiation_Time; //ACT-R reference manual: The first 50 ms (by default) of the movement is movement initiation. 
                }
                
                sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(Move_Entity.Chunk.Slot, "motor_execution_time", (Double.valueOf((String)Move_Entity.Chunk.Slot.get("motor_execution_time")) + init_time) ) ;
               
                
              }
              
            }
            
            
            break;
            
          case Ending:
            
            if(!Entity.Trash ) {
              
              if (Entity.From.equals( "Motor Preparation") && Entity.To.equals( "Motor Initiation") && Entity.Entity_Type.equals( "Add Manual")){
                String request_type = Entity.Chunk.Chunk_Type;
                String details = "";
                if( request_type.equals( "press-key")) details += "press-key " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                if( request_type.equals( "customized-manual-action")) details += "customized-manual-action " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "name");
                if( request_type.equals( "clear")) details += "clear";
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "INITIATION-COMPLETE " + details); 
                Entity.From = "Motor Initiation"; //tag from and to
                Entity.To = "Motor Execution";
              }
              
              int itemsInTask22 = sim.funs.getNumberOfQnactrEntityInServer("motorpreparation");
              int itemsInQueue22 = sim.funs.getNumberOfQnactrEntityInQueue("motorpreparation");
              int itemsInTask23 = sim.funs.getNumberOfQnactrEntityInServer("motorinitiation");
              int itemsInQueue23 = sim.funs.getNumberOfQnactrEntityInQueue("motorinitiation");
              
              if(itemsInQueue23 == 0 && itemsInTask23 == 1 && itemsInQueue22 == 0 && itemsInTask22 == 0 )sim.vars.motorModule.Processor_Free = true;
              //System.out.println(GlobalUtilities.round(SimSystem.clock(),3) + "sim.vars.motorModule.Processor_Free: " + sim.vars.motorModule.Processor_Free);
              
            }
            
            break;
            
          case Timing:
            
            if(Entity.Trash ) return 0.0;
            
            Entity.Time_Computed = true;
            if (Entity.Chunk.Chunk_Type.equals( "world3d-driving-two-point-visual-manual-steer")) return 0.0;
            else if (Entity.Chunk.Chunk_Type.equals( "customized-manual-action" )) {
              String initiation_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "initiation-duration");
              if( !sim.funs.ProgramUtilitiesFun__Is_String_Double (initiation_duration) ) {
                System.out.println("Error! Motor Initiation must be a double rather than: " + initiation_duration);	
                SimSystem.abort();
              }
              
              return Double.parseDouble( initiation_duration );
            }
            else {
              if(Entity.Chunk.Slot.containsKey("motor_initiation_time") ) { //use customized time
                double motor_initiation_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_initiation_time" ) );
                return motor_initiation_time;
              }
              else	return sim.vars.motorModule.Motor_Initiation_Time; //ACT-R reference manual: The first 50 ms (by default) of the movement is movement initiation. 
            }
            
        }
        break;
        
      case motormodule:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
          case Beginning:

            if(!Entity.Trash ) {
              
              // set last command
              sim.vars.motorModule.Last_Command_Chunk_Type = Entity.Chunk.Chunk_Type;
              
              Entity.Time_Computed = false;
              if (Entity.From.equals( "Execution" ) && Entity.To.equals( "Motor Module") && Entity.Entity_Type.equals( "Add Manual")){
                //            			if(Entity.Chunk.Chunk_Type.equals( "" )){
                //            				System.out.println("Motor Module Add Manual Chunk Type empty.");
                //            			}
                //            			else{
                
                // turn prepare into normal action
                if(Entity.Chunk.Chunk_Type.equals( "prepare")) {
                  // TODO error check
                  // set type to style slot value
                  Entity.Chunk.Chunk_Type = (String)Entity.Chunk.Slot.get("style");
                  // add indicator for no execution
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(Entity.Chunk, "immediate-execute", "false");
                }
                
                String request_type = Entity.Chunk.Chunk_Type;
                switch(request_type){ //defmethod pm-module-request in motor.lisp
                  case "press-key":{ 
                    //isa press-key
                    //key key
                    String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                    if(key.equals( "" )) System.out.println("Motor Module press-key has key == empty ");
                    if (key.charAt(0) == '\"') key = key.substring(1);
                    if (key.charAt(key.length()-1) == '\"') key = key.substring(0, key.length()-1);
                    key = key.toLowerCase();
                    sim.funs.ChunkFun__Set_Chunk_Slot_Value(Entity.Chunk, "key", key);
                    //System.out.println(key);
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + Entity.Chunk.Chunk_Type + " " + key); //suppose the key content is always the first slot
                    if(sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.containsKey ( key ) == false){
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("#|Warning: No press-key mapping available for key " + key  +". |#");
                      Entity.Entity_Type = "";
                      Entity.From = "";
                      Entity.To = "";
                      Entity.Trash = true;
                    }
                    

                    break;
                  }
                  case "punch":{ //e.g. isa punch      hand  right      finger middle
                    String hand = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                    String finger = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finger");
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tPUNCH HAND " + hand + ", FINGER " + finger); //suppose the key content is always the first slot
                    break;
                  }
                  case "tap":{ //e.g. isa tap      hand  right      finger middle
                      String hand = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                      String finger = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finger");
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tTAP HAND " + hand + ", FINGER " + finger); //suppose the key content is always the first slot
                      break;
                  }
                  
                  case "click-mouse": { //Clicking the mouse is really just a punch with the right index finger. ACT-R 6.0 motor.lisp
                    String hand = "right";
                    String finger = "index";
                    
                    if( !sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals( "mouse" )) { 
                      System.out.println( " #|Warning: CLICK-MOUSE requested when hand not at mouse #(28, 2)! |# " );
                      Entity.Trash = true;
                      break;
                    }
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tCLICK-MOUSE i.e., PUNCH HAND " + hand + ", FINGER " + finger);
                    break;
                  }
                  
                  case "move-cursor": { //move cursor to a visual icon chunk (visicon) or a visual location.
                    if(! sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals( "mouse" )) { 
                      System.out.println( " #|Warning: move-cursor requested when hand not at mouse #(28, 2)! |# " );
                      Entity.Trash = true;
                      break;
                    }
                    //	Isa move-cursor
                    // 	[ object object | loc location ]
                    // 	{device [ mouse | joystick-1 | joystick-2]}
                    
                    //move-cursor just moves the cursor on the screen, and does not change the hand location. The location is still #(28,2) for using device mouse, which can be tested in ACT-R with (loc (right-hand (get-module :motor)) )
                    
                    //String hand = "right"; //move the cursor with the right hand by default.
                    String object_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "object");
                    String loc_string    = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "loc");
                    if( (object_string.equals( "nil") && loc_string.equals( "nil" ))  ) { // ACT-R first use the visual object to get the screen-location, if no visual object (visicon) specified in the move-cursor request, use the specified visual-location, if no visual-location, return error.   ACT-R 6.0 motor.lisp: ";; always refer back to the visicon chunks if possible"
                      System.out.println ( "Error! Motor Module move-cursor must move to an object (preferred) or a location, but OBJ: " + object_string + ", and LOC: " +loc_string);
                      Entity.Trash = true;
                      break;
                    }	
                    
                    String device_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "device");
                    if (device_string.equals( "nil") ) {
                      device_string = "mouse" ; //by default
                      sim.funs.ChunkFun__Set_Chunk_Slot_Value 	( Entity.Chunk , "device" , "mouse" );
                    }
                    
                    int old_cursor_x = sim.vars.deviceModule.Mouse_Cursor_Screen_X;
                    int old_cursor_y = sim.vars.deviceModule.Mouse_Cursor_Screen_Y;		
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tMOVE-CURSOR FROM screen point (" + old_cursor_x + ", " + old_cursor_y + ") TO OBJ OR LOC: OBJ " + object_string + ", LOC " + loc_string + " WITH DEVICE " + device_string );
                    
                    //check for movement distance == 0.
                    Chunk hfrt_chunk = sim.funs.MotorModuleFun__Motor_Request_Chunk_To_Motor_Computation_Chunk (Entity.Chunk );
                    String movement_distance_r_in_visual_angle_degree = sim.funs.ChunkFun__Get_Chunk_Slot_Value( hfrt_chunk, "r" );
                    if( Double.parseDouble( movement_distance_r_in_visual_angle_degree ) == 0.0 ){
                      Entity.Trash = true;
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tMOVE-CURSOR distance = 0, so omit the movement." );
                    }
                    
                    break;
                  }
                  
                  case "move-hand-touch": { //move hand (left or right) to a visual icon chunk (visicon) or a visual location.
                	  
                	  //ACT-Touch can only move the right hand, here Cao added hand slot
                	  //Isa move-hand-touch
                      //[ object object | loc location  ]
                	  //{hand [ left | right]}
                	  
                	  //move-hand-touch moves the hand location.
                	  String hand = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                	  //safety check
                	  if (hand.equals("left") && !sim.vars.motorModule.Left_Hand.Object_Type_In_Hand.equals("touchscreen")) System.out.println ( "Error! ServerLogics motormodule has a move-hand-touch request hand: " + hand + ", but sim.vars.motorModule.Left_Hand.Object_Type_In_Hand is: " + sim.vars.motorModule.Left_Hand.Object_Type_In_Hand);
                	  if (hand.equals("right") && !sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals("touchscreen")) System.out.println ( "Error! ServerLogics motormodule has a move-hand-touch request hand: " + hand + ", but sim.vars.motorModule.Right_Hand.Object_Type_In_Hand is: " + sim.vars.motorModule.Right_Hand.Object_Type_In_Hand);
                	  
                	  String object_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "object");
                      String loc_string    = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "loc");
                      if( (object_string.equals( "nil") && loc_string.equals( "nil" ))  ) { // ACT-R first use the visual object to get the screen-location, if no visual object (visicon) specified in the request, use the specified visual-location, if no visual-location, return error.   ACT-R 6.0 motor.lisp: ";; always refer back to the visicon chunks if possible"
                        System.out.println ( "Error! Motor Module move-hand-touch must move to an object (preferred) or a location, but OBJ: " + object_string + ", and LOC: " +loc_string);
                        Entity.Trash = true;
                        break;
                      }	
     
                      
                      Motor_Module_Hand handObj = null; 
                	  if(hand.equals("right")){                	  
	                      if(! sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals( "touchscreen" )) { 
	                        System.out.println( " #|Warning: move-hand-touch requested when right hand not at touchscreen! |# " );
	                        Entity.Trash = true;
	                        break;
	                      }
	                      
	                      handObj = sim.vars.motorModule.Right_Hand;
                	  }
                	  
                	  if(hand.equals("left")){                	  
	                      if(! sim.vars.motorModule.Left_Hand.Object_Type_In_Hand.equals( "touchscreen" )) { 
	                        System.out.println( " #|Warning: move-hand-touch requested when left hand not at touchscreen! |# " );
	                        Entity.Trash = true;
	                        break;
	                      }
	                      
	                      handObj = sim.vars.motorModule.Left_Hand;
                	  }
                      
                                             
                      int old_hand_x_px = sim.funs.MotorModuleFun__Get_Move_Hand_Touch_Latest_Move_To_Location_X(handObj); //(int) handObj.Location.Ob1;
                      int old_hand_y_px = sim.funs.MotorModuleFun__Get_Move_Hand_Touch_Latest_Move_To_Location_Y(handObj); //(int) handObj.Location.Ob2;
                      
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tMOVE-HAND-TOUCH FROM screen point (" + old_hand_x_px + ", " + old_hand_y_px + ") TO OBJ OR LOC: OBJ " + object_string + ", LOC " + loc_string + " WITH " + hand + " HAND." );
                      
                      
                      
                      //check for movement distance == 0.
                      Chunk hfrt_chunk = sim.funs.MotorModuleFun__Motor_Request_Chunk_To_Motor_Computation_Chunk (Entity.Chunk );
                      String movement_distance_r_in_visual_angle_degree = sim.funs.ChunkFun__Get_Chunk_Slot_Value( hfrt_chunk, "r" );
                      if( Double.parseDouble( movement_distance_r_in_visual_angle_degree ) == 0.0 ){
                        Entity.Trash = true;
                        sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\tMOVE-HAND-TOUCH distance = 0, so omit the movement." );
                      }
                      
                      
                      //System.out.println("handObj.Latest_Move_To_Location.Ob1: " + handObj.Latest_Move_To_Location.Ob1.equals("")); 
                      
                      break;
                    }                  
                  
                		  
                		  
                  case "world3d-driving-two-point-visual-manual-steer": { 
                    World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                    double  na_old = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "na-old") );
                    double  na_new = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "na-new"));
                    double  fa_old = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "fa-old"));
                    double  fa_new = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "fa-new"));
                    double  time_old = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "time-old"));
                    double  time_new = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "time-new"));
                    
                    if(sim.vars.animator3DModule.Show_Animator3D){
                      System.out.println("TODO serverlogics case motormodule Beginning: Animator3D");
//TODO     Animator3D     
//                      Animator3D.SetCommentText(11, "Near Point Angle (degree): " + Double.toString(GlobalUtilities.round(na_new,3)));
//                      Animator3D.SetCommentText(12, "Far Point Angle (degree): " + Double.toString(GlobalUtilities.round(fa_new,3)));
                    }
                    
//                    method_pointer.Simulation = this;
                    double  delta_steer_angle = method_pointer.Get_Delta_Steer(na_old, na_new, fa_old, fa_new, time_old, time_new);
                    sim.funs.ChunkFun__Set_Chunk_Slot_Value(Entity.Chunk, "delta-steer", Double.toString(delta_steer_angle) );
                    
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t start Manual STEERING " + delta_steer_angle + " degree, with new near angle: " + na_new + ", and new far angle: " + fa_new);
                    break;
                  } //end of "world3d-driving-two-point-visual-manual-steer"
                  
                  case "customized-manual-action":{
                    String name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "name" );
                    String preparation_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "preparation-duration");
                    String initiation_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "initiation-duration");
                    String execution_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "execution-duration" );
                    String finish_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finish-duration" );
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t start Manual customized-manual-action: " + name + ", preparation-duration: " + preparation_duration + ", initiation-duration: " + initiation_duration + ", execution-duration: " + execution_duration + ", finish-duration: "+ finish_duration );
                    break;
                  } //end of "customized-manual-action"
                  
                  case "world3d-sp-driving-control-manual-joystick":{
                    String axis = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "axis");
                    String value = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "value");
                    String duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "duration" );
                    
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t start Manual joystick sp-driving-control, axis: " + axis + ", value: " + value + ", duration: " + duration);
                    break;
                  }
                  
                  case "type-letters":{
                    //break down typing letters into a queue of press-key 
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t break down typing letters into a queue of press-key");
                    sim.funs.MotorModuleFun__Motor_Request_Type_Letters_To_Press_Keys( Entity.Chunk );  //will cast press-key entities in Motor Module
                    Entity.Trash = true; //end this entity
                    
                    break;
                  }
                  case "clear":{
                    //clear motor queues
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t clear");
                    break;
                  }
                  case "execute": {
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t execute");
                    break;
                  }
                  case "":{
                    System.out.println("Motor Module beginning effect has an empty ISA command type.");
                    break;
                  }
                  default: {
                    System.out.println("Motor Module beginning effect has undefine case: " + request_type);
                    break;
                  }
                }
              }
              
              if(Entity.Trash == false) sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.utilization__Motor_Module_Changes_In_A_Second, (double)GlobalUtilities.round (SimSystem.clock(),3) , 1.0 ) ; 
              
              
            }
            
            break;
            
          case Ending:
            
            if(!Entity.Trash ) {
              
              if (Entity.Trash == false && Entity.Entity_Type.equals( "Add Manual")) { 
                Entity.From = "Motor Module"; //tag from and to
                Entity.To = "Motor Preparation";
              }
              // set up execute entities for initiation
              if (Entity.Chunk.Chunk_Type.equals( "execute")) {
                Entity.From = "Motor Module";
                Entity.To = "Motor Initiation";
                // TODO clone the chunk?
                Entity.Chunk = sim.vars.motorModule.Last_Prep;
                // TODO since we're skipping preparation, we need to make sure we set the buffer states that would be set there
              }
              
            }
            
            break;
            
          case Timing:

            
            if(Entity.Trash ) return 0.0;
            
            //although the timing for this task node is 0, compute initiation time, execution time, and finish time here rather than later in each node,
            //because if execution time uses rand-time, finish time must use the same rand-time.
            
            Entity.Time_Computed = true;
            
            if( Entity.Trash ) return 0.0; 
            
            //System.out.println("Motor Module Timing, " + Entity.Chunk.Chunk_Type + " " + Entity.Trash);
            
            Chunk command = new Chunk();
            if (Entity.Entity_Type.equals( "Add Manual")){
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                case "press-key":{ //become either (chunk-type punch hand finger) = (defStyle punch () hand finger) or (chunk-type peck-recoil hand finger r theta) = PECK-RECOIL class which is a sub sytle of PECK of (defStyle hfrt-movement () hand finger r theta)
                  String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                  command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.get(key);
                  break;
                }
                
                case "click-mouse": { //Clicking the mouse is really just a punch with the right index finger. ACT-R 6.0 motor.lisp
                  command = sim.funs.ChunkFun__Make_Chunk_From_Descritption(  sim.funs.ProgramUtilitiesFun__String_To_StringArray("isa punch hand right finger index ")  );
                  break;
                }
                
                case "punch":	     //(chunk-type punch hand finger) = (defStyle punch () hand finger)
                case "tap":	
                case "move-cursor": //move cursor to a visual icon chunk (visicon) or a visual location.
                case "move-hand-touch":
                case "world3d-driving-two-point-visual-manual-steer":	
                case "customized-manual-action":
                { //common method
                  command = Entity.Chunk; //use pointer rather than clone, because sim.funs.MotorModuleFun__Compute_Execution_Time may add some computed values in the Chunk slots to save some further computation.
                  break;
                }
                
                case "world3d-sp-driving-control-manual-joystick":	{
                  //temp method
                  double temp_preparation_time = 0.100; //may be further developed based on movement feature preparation
                  double after_initiation_till_control_device_gets_action_time = 0.100; // the hand movement time needed to hit the joystick key
                  String press_duration_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "duration" );
                  if ( ! sim.funs.ProgramUtilitiesFun__Is_String_Double(press_duration_string) ) {
                    System.out.println("Error! Motor Module timing has ! sim.funs.ProgramUtilitiesFun__Is_String_Double(press_duration), press_duration: " + press_duration_string );
                    SimSystem.abort();
                  }
                  double press_duration = Double.parseDouble ( press_duration_string );
                  double temp_motor_finish_time = after_initiation_till_control_device_gets_action_time + press_duration + after_initiation_till_control_device_gets_action_time;
                  
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_preparation_time", Double.toString(temp_preparation_time) );
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_execution_time", Double.toString(after_initiation_till_control_device_gets_action_time) ); //after initiation till control device gets action
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_finish_time", Double.toString(temp_motor_finish_time) ); //after initiation till motor execution ready for the next action
                  
                  return 0.0;

                }
                
                case "clear":{
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_preparation_time", "0.0" );
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_execution_time", "0.0" ); //after initiation till control device gets action
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_finish_time", "0.0" ); //after initiation till motor execution ready for the next action
                  
                  return 0.0;
                }
                case "execute":
                  // do nothing here. we will be resuing the chunk in Last_Prep which already has times computed
                  // TODO we should be recomputing the times because they are random
                  // TODO we should also be setting last-command (not Last_Prep, which should be called last-prep)
                  // return so we don't go through all the computation function calls below
                  return 0.0;
                case "":{
                  System.out.println("Motor module timing has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Motor module timing has undefine case: " + request_type);
                  break;
                }	
              }
            }
            
            //preparation time/duration
            double motor_preparation_time;
            if(Entity.Chunk.Chunk_Type.equals( "customized-manual-action")){
              String preparation_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "preparation-duration");
              if( !sim.funs.ProgramUtilitiesFun__Is_String_Double (preparation_duration) ) {
                System.out.println("Error! Motor Module timing need preparation_duration to be a double rather than: " + preparation_duration);	
                SimSystem.abort();
              }
              motor_preparation_time = 	Double.parseDouble( preparation_duration );
              
            }
            else	motor_preparation_time = sim.funs.MotorModuleFun__Compute_Preparation_Time(command); //time needed to prepare the movement
            
            //System.out.println("1: "+ motor_preparation_time );
            if(sim.vars.motorModule.Is_Typing_Letters) {
              String request_type = Entity.Chunk.Chunk_Type;
              if(request_type.equals( "press-key")){
                String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                motor_preparation_time = sim.funs.MotorModuleFun__Typing_Motor_Time_Offset( (double) motor_preparation_time , key);
              }
            }
            //System.out.println( "2: " + motor_preparation_time );
            
            sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_preparation_time", Double.toString(motor_preparation_time) );
            
            
            
            
            double motor_initiation_time = sim.vars.motorModule.Motor_Initiation_Time;
            if(sim.vars.motorModule.Is_Typing_Letters) {
              String request_type = Entity.Chunk.Chunk_Type;
              if(request_type.equals( "press-key")){
                String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                motor_initiation_time = sim.funs.MotorModuleFun__Typing_Motor_Time_Offset((double) motor_initiation_time , key);
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_initiation_time", Double.toString(motor_initiation_time) );
              }
            }
            //if "motor_initiation_time" is not specified, initiation time is still computed in initiation node, because it is the same for all motor commands, except for customized-manual-action
            
            
            
            //execution time/duration, this is the time between the start of execution and the moment that control devices get any action like key pressed down.
            double motor_execution_time;
            if(Entity.Chunk.Chunk_Type.equals( "customized-manual-action")){
              String execution_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "execution-duration" );
              if( !sim.funs.ProgramUtilitiesFun__Is_String_Double (execution_duration) ) {
                System.out.println("Error! Motor Module timing need execution_duration to be a double rather than: " + execution_duration);	
                SimSystem.abort();
              }
              motor_execution_time = 	Double.parseDouble( execution_duration );
              
            }
            else motor_execution_time	= sim.funs.MotorModuleFun__Compute_Execution_Time(command);
            sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_execution_time", Double.toString(motor_execution_time) );
            // if move-cursor and increment mouse moves, calculate times for incremental movements
            // TODO do we have to also check for chunk type ply?
            if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && !sim.vars.motorModule.Incremental_Mouse_Moves.equals( "nil")) {
              sim.funs.MotorModuleFun__Compute_Incremental_Mouse_Moves(Entity.Chunk);
            }
            
            
            
            //finish time/duration between execution start and ready for the next movement.  need to be after execution time, because it will use the same execution time (if there is any random factor in the execution time).
            double motor_finish_time;
            if(Entity.Chunk.Chunk_Type.equals( "customized-manual-action")){
              String finish_duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finish-duration" );
              if( !sim.funs.ProgramUtilitiesFun__Is_String_Double (finish_duration) ) {
                System.out.println("Error! Motor Module timing need finish_duration to be a double rather than: " + finish_duration);	
                SimSystem.abort();
              }
              motor_finish_time = 	Double.parseDouble( finish_duration );
              
            }
            else motor_finish_time = sim.funs.MotorModuleFun__Compute_Finish_Time(command, Entity);
            sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value( Entity.Chunk, "motor_finish_time", Double.toString(motor_finish_time) );
            
            sim.vars.motorModule.Last_Prep = command; //do this after Compute_Preparation_Time  // Chris
            sim.vars.motorModule.Last_Command = command; //do this after Compute_Preparation_Time // Shi
            
            
            if(sim.vars.motorModule.Is_Typing_Letters) {
              String request_type = Entity.Chunk.Chunk_Type;
              if(request_type.equals( "press-key")){	
                String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                sim.vars.motorModule.Typing_Data_Last_Pressed_Key = key;
              }
            }
            
            double motor_subtnetwork_time = motor_preparation_time + motor_initiation_time;
            //System.out.println(motor_subtnetwork_time);
            if(sim.vars.motorModule.Enable_Motor_Subnetwork_Decay){
              double p_this = Math.pow(2, -1.0 * ( motor_subtnetwork_time / sim.vars.motorModule.Motor_Subnetwork_Decay_Half_Life ) );
              double p_random = Math.random();
              
              Entity.Note = Double.toString(motor_subtnetwork_time);
              
              if (p_random <  p_this) {
                // no decay, do nothing
              }
              else{
                //Entity.Note = "motor_subnetwork_decay" ;
                //System.out.println("motor_subnetwork_decay1" );
              }
            }
            
            
            return 0.0; 
            
        }
        break;
        
      case motorpreparation:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
          case Beginning:
            
            if(!Entity.Trash ) {
              
              //Entity.Time = 0.0;
              Entity.Time_Computed = false;
              //1. beginning effect; 2. timing; 3. ending effect
              sim.vars.motorModule.State_Free = false;
              sim.vars.motorModule.Preparation_Free = false; 
              //System.out.println(GlobalUtilities.round(SimSystem.clock(),3) + "sim.vars.motorModule.Preparation_Free: " + sim.vars.motorModule.Preparation_Free);
              
              sim.vars.motorModule.Processor_Free = false;
              //System.out.println(GlobalUtilities.round(SimSystem.clock(),3) + "sim.vars.motorModule.Processor_Free: " + sim.vars.motorModule.Processor_Free);
              
            }
            
            break;
            
          case Ending:
            
            if(!Entity.Trash ) {
              
              if (Entity.From.equals( "Motor Module") && Entity.To.equals( "Motor Preparation") && Entity.Entity_Type.equals( "Add Manual")){
                
                String request_type = Entity.Chunk.Chunk_Type;
                switch(request_type){
                  case "press-key":
                  case "punch":
                  case "tap":
                  case "click-mouse":
                  case "move-cursor":
                  case "move-hand-touch":
                  case "world3d-driving-two-point-visual-manual-steer":	
                  case "customized-manual-action":
                  case "world3d-sp-driving-control-manual-joystick":
                  case "clear":
                  { 
                    String details = "";
                    if( request_type.equals( "press-key")) details += "press-key " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                    if( request_type.equals( "customized-manual-action")) details += "customized-manual-action " + sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "name");
                    if( request_type.equals( "clear") ) details += "clear";
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "PREPARATION-COMPLETE " + details); 
                    break;
                  }
                  
                  case "":{
                    System.out.println("Motor preparation ending effect has an empty ISA command type.");
                    break;
                  }
                  default: {
                    System.out.println("Motor preparation ending effect has undefine case: " + request_type);
                    break;
                  }	
                }
              }


              int itemsInTask22 = sim.funs.getNumberOfQnactrEntityInServer("motorpreparation");
              int itemsInQueue22 = sim.funs.getNumberOfQnactrEntityInQueue("motorpreparation");
              if(itemsInQueue22 == 0 && itemsInTask22 == 1)sim.vars.motorModule.Preparation_Free = true;
              //System.out.println(GlobalUtilities.round(SimSystem.clock(),3) + "sim.vars.motorModule.Preparation_Free: " + sim.vars.motorModule.Preparation_Free);
              
              
              Entity.From = "Motor Preparation"; //tag from and to
              Entity.To = "Motor Initiation";
              
            }
            break;
            
          case Timing:
            if(Entity.Trash ) return 0.0;
            
            double motor_preparation_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_preparation_time" ) );
            Entity.Time_Computed = true;
            //System.out.println( "3: " + motor_preparation_time );
            return motor_preparation_time;
        }
        break;
        
      case retrievalbuffer:
        switch (ServiceStage){
          case Release:
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return  (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
          
          case Beginning:
            Entity.Time_Computed = false;
            if (Entity.Entity_Type.equals( "Retrieval Buffer's New Chunk") ){
              if (Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" )){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "DECLARATIVE" + "\t" + "RETRIEVAL-FAILURE");
                sim.vars.retrievalBuffer.Empty = true;
              }
              else {
                //renaming and define-chunk for the retrieval buffer chunk. The retrieved chunk must have a name, so do not define-chunk it again.
                //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
                int j = 0;
                String old_chunk_name = Entity.Chunk.Chunk_Name;
                String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
                while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                  j++;
                  new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
                }
                Entity.Chunk.Chunk_Name = new_chunk_name;
                if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                  Entity.Chunk.Activation = (double) 0.0; //all default values
                  Entity.Chunk.Creation_Time = 0.0;
                  Entity.Chunk.Number_Of_Presentations = 0;
                  Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
                  if (Entity.Chunk.DM_Name_Origin.equals( "" ) ) {
                    //GlobalUtilities.popUpMessage("Retrieval Buffer old_chunk_name: " + old_chunk_name + " new_chunk_name: " + new_chunk_name);
                    Entity.Chunk.DM_Name_Origin = old_chunk_name; //update the DM_Name_Origin for =retrieval variable in buffer action
                  }
                  else {
                    //nothing
                  }
                }
                sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "DECLARATIVE" + "\t" + "SET-BUFFER-CHUNK RETRIEVAL " + Entity.Chunk.Chunk_Name); 
                sim.vars.retrievalBuffer.Empty = false;
              }	
              sim.vars.retrievalBuffer.Retrieval_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
            }
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Retrieval_Buffer_Contents();
            
            break;
            
          case Ending:
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;
            
            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
        }
        break;
        
      case speechexecution:
        switch (ServiceStage){
          case Release:
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //1. beginning effect; 2. timing; 3. ending effect
            if (Entity.From.equals( "Speech Initiation") && Entity.To.equals( "Speech Execution") && Entity.Entity_Type.equals( "Add Vocal")){ //copy an entity to control for Speech execution results
              if(!Entity.Chunk.Chunk_Type.equals( "clear") && !Entity.Chunk.Chunk_Type.equals( "subvocalize")){ //subvocalize should not have output sound
                Entity Temp_Entity = sim.funs.createEntity( "Control Voice Key" , "Speech Execution", "Control Voice Key", "Speech Execution Result", 0.0);
                //                Entity Temp_Entity = new Entity();  
                //                Temp_Entity.ID = "222"; //Control Voice Key
                //                Temp_Entity.Time = (double) SimSystem.clock();
                //                Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
                //                QnactrSimulation.entityNumber++;
                //                Temp_Entity.From = "Speech Execution"; //tag from and to
                //                Temp_Entity.To = "Control Voice Key";
                //                Temp_Entity.Entity_Type = "Speech Execution Result";   
                Temp_Entity.Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              }
              else if (Entity.Chunk.Chunk_Type.equals( "subvocalize")){ //talk to oneself
                String content = (String) Entity.Chunk.Slot.get("string");
                double duration = sim.funs.SpeechModuleFun__Get_Articulation_Time(content);
                //System.out.println("Speech execution subvocalize duration: " + duration);
                double content_delay = sim.vars.speechModule.Subvocalize_Detect_Delay;
                double recode_time = 0.0;
                double onset_clock_time_value = SimSystem.clock() + 0.0 ; //currently ACT-R speech execution time is 0.0;
                String location = "internal";
                String kind = "word";
                sim.funs.DeviceModuleFun__Audio_Display_Prepare_Other_Sound(content, Double.toString(duration), (double) content_delay, (double) recode_time, Double.toString(onset_clock_time_value), location, kind); //content, duration, content_delay, recode_time, onset_clock_time(optional, Clock by default), location (optional, "external" by default), kind (optional "speech" by default)
              }
            }
            
            break;
            
          case Ending:
            
            //here the speech movement is completed.
            if (Entity.From.equals( "Speech Initiation") && Entity.To.equals( "Speech Execution") && Entity.Entity_Type.equals( "Add Vocal")){
              if (!Entity.Chunk.Chunk_Type.equals( "clear"))sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "SPEECH" + "\t\t" + "FINISH-MOVEMENT"); 
              sim.funs.AnimatorModuleFun__Complete_Speech( );
              
              
            }
            else System.out.println( "Speech Execution ending effect has undefined case");
            
            sim.vars.speechModule.Execution_Free = true;
            if(sim.vars.speechModule.Preparation_Free && sim.vars.speechModule.Processor_Free && sim.vars.speechModule.Execution_Free )   sim.vars.speechModule.State_Free = true;
            Entity.From = "Speech Execution"; //tag from and to
            Entity.To = "";
            Entity.Entity_Type = "Speech Finish";
            
            
            if(sim.vars.speechModule.State_Free)  sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Speech_Module_Changes_In_A_Second , (double)GlobalUtilities.round (SimSystem.clock(),3) , 0.0 );
            
            
            
            break;
            
          case Timing:
            double speech_finish_time = 0.0;
            Chunk command = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
            speech_finish_time = sim.funs.SpeechModuleFun__Compute_Finish_Time(command);
            
            //GlobalUtilities.popUpMessage(Clock + " " + speech_finish_time);
            //GlobalUtilities.popUpMessage(Clock + " " + Entity.Time);
            Entity.Time_Computed = true;
            return speech_finish_time;
        }
        break;
 
        
      case speechinitiation:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
                        
          case Beginning:
            
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //1. beginning effect; 2. timing; 3. ending effect
            sim.vars.speechModule.Execution_Free = false;
            
            break;
            
          case Ending:
            
            if (Entity.From.equals( "Speech Preparation") && Entity.To.equals( "Speech Initiation") && Entity.Entity_Type.equals( "Add Vocal")){
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "SPEECH" + "\t\t" + "INITIATION-COMPLETE"); 
              Entity.From = "Speech Initiation"; //tag from and to
              Entity.To = "Speech Execution";
            }
            sim.vars.speechModule.Processor_Free = true;
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return sim.vars.speechModule.Speech_Initiation_Time; //ACT-R reference manual: The first 50 ms (by default) of that output is initiation.
            
        }
        break;

      case speechmodule:
        switch (ServiceStage) {
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
            
          case Beginning:
            
            Entity.Time_Computed = false;
            
            sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.utilization__Speech_Module_Changes_In_A_Second , (double)GlobalUtilities.round (SimSystem.clock(),3) , 1.0 ) ; 
            
            
            //Entity.Time = 0.0;
            //modifed from Motor Module
            if (Entity.From.equals( "Execution") && Entity.To.equals( "Speech Module" ) && Entity.Entity_Type.equals( "Add Vocal")){
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){ //defmethod pm-module-request in speech.lisp there are clear, speak, and subvocalize
                case "speak":
                case "subvocalize":
                { 
                  //isa speak
                  //String "5"
                  String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "string");
                  if(key.equals( "" )) System.out.println("Speech Module speak has key == empty ");
                  if (key.charAt(0) == '\"') key = key.substring(1);
                  if (key.charAt(key.length()-1) == '\"') key = key.substring(0, key.length()-1);
                  key = key.toLowerCase();
                  sim.funs.ChunkFun__Set_Chunk_Slot_Value(Entity.Chunk, "string", key);
                  //System.out.println(key);
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "SPEECH" + "\t\t" + Entity.Chunk.Chunk_Type + " TEXT " + key); //suppose the key content is always the first slot
                  
                  //        					if(sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.ContainsKey ( key ) == false){
                  //        						ProgramUtilitiesFun__Output_Trace_Txt("#|Warning: No press-key mapping available for key " + key  +". |#");
                  //        						Entity.Entity_Type = "";
                  //        						Entity.From = "";
                  //        						Entity.To = "";
                  //        						Entity.Trash = true;
                }
                
                break;
                case "":{
                  System.out.println("Speech Module beginning effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Speech Module beginning effect has undefine case: " + request_type);
                  break;
                }
              }
            }
            
        
          case Ending:
            if (Entity.Entity_Type.equals( "Add Vocal")) { 
              Entity.From = "Speech Module"; //tag from and to
              Entity.To = "Speech Preparation";
            }
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
        }
        break;
        

        
      case speechpreparation:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (itemsInTask < 1);
                        
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //1. beginning effect; 2. timing; 3. ending effect
            sim.vars.speechModule.State_Free = false;
            sim.vars.speechModule.Preparation_Free = false;
            sim.vars.speechModule.Processor_Free = false;
            
            break;
            
          case Ending:
            
            
            //modified from motor preparation
            if (Entity.From.equals( "Speech Module") && Entity.To.equals( "Speech Preparation" ) && Entity.Entity_Type.equals( "Add Vocal")){
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                case "speak":
                case "subvocalize":
                { 
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "SPEECH" + "\t\t" + "PREPARATION-COMPLETE"); 
                  break;
                }
              
                case "":{
                  System.out.println("Speech preparation ending effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Speech preparation ending effect has undefine case: " + request_type);
                  break;
                }	
              }
            }
            
            sim.vars.speechModule.Preparation_Free = true;
            Entity.From = "Speech Preparation"; //tag from and to
            Entity.To = "Speech Initiation";
            
            
            break;

          case Timing:
            double speech_preparation_time = 0.0;
            Chunk command = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
            speech_preparation_time = sim.funs.SpeechModuleFun__Compute_Preparation_Time(command); //time needed to prepare the speech 
            sim.vars.speechModule.Last_Command = command;
            Entity.Time_Computed = true;
            return speech_preparation_time;

        }
        break;
        
      case temporalbuffer:
        switch (ServiceStage){
          case Release:
            
            return true;
            
            
          case Beginning:
            
            // No Beginning effect
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Entity.Entity_Type: " + Entity.Entity_Type + ", Entity.Tag: " + Entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            // clear buffer and other actions moved to temporalmodule
            
            
            
            Entity.From = "Temporal Buffer"; //change the entity and pass it to Matching And Selection as a trigger
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
          //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; 
            //QnactrSimulation.entityNumber++;
            sim.funs.NetworkDetailsVisualizationFun__Get_Temporal_Buffer_Contents();
            
            break;
            
          case Timing:
            return 0.0;
            	
        }
        break;
 
        
      case temporalmodule:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
            
          case Beginning:
            Entity.Time_Computed = false;
            
            // set clear properties
            if(Entity.Chunk.Chunk_Type.equals( "clear")) {
              Entity.Entity_Type = "Clear Temporal";
            }
            
            
            break;
            
          case Ending:
            //if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Entity.Entity_Type: " + Entity.Entity_Type + ", Entity.Tag: " + Entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            // turn Request Temporal into Increment Temporal
            if (Entity.Entity_Type.equals( "Request Temporal")) {
              Entity.Entity_Type = "Increment Temporal";
            }
            // turn Add Temporal into Request Temporal
            if (Entity.Entity_Type.equals( "Add Temporal")) {
              Entity.Entity_Type = "Request Temporal";
            }
            
            // update increment entity
            
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
            //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
            //QnactrSimulation.entityNumber++;
            
            Entity.From = "Temporal Module";
            
            
            // if this is a request, kill any other active entities looping around
            if(Entity.Entity_Type.equals( "Request Temporal")) {
              
              //all entities in "602" Temporal Module
              LinkedList<Entity> entityList1 = sim.funs.getLinkedListOfQnactrEntityInServer("temporalmodule");
              LinkedList<Integer> abortEntityTags = new LinkedList<Integer>();
              Iterator<Entity> entities = entityList1.iterator();
              while(entities.hasNext()){
                Entity entity = entities.next();
                
                if(entity.Tag == Entity.Tag) continue; // do not abort self.
                
                if(entity.Entity_Type == "Request Temporal" || entity.Entity_Type == "Increment Temporal" || entity.Entity_Type == "Add Temporal"){
                  
                  if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Temporal Module removing entity.Entity_Type: " + entity.Entity_Type + ", entity.Tag: " + entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
                  abortEntityTags.addLast(entity.Tag);
                  
                }
              }
              for(Integer entityTag : abortEntityTags){
                
                sim.funs.abortEntityInServer("temporalmodule", entityTag);
              }
              
              
              //all entities in "603" Temporal Buffer
              LinkedList<Entity> entityList2 = sim.funs.getLinkedListOfQnactrEntityInServer("temporalbuffer");
              LinkedList<Integer> abortEntityTags2 = new LinkedList<Integer>();
              Iterator<Entity> entities2 = entityList2.iterator();
              while(entities2.hasNext()){
                Entity entity = entities2.next();
                
                if(entity.Tag == Entity.Tag) continue; // do not abort self.
                
                if(entity.Entity_Type == "Request Temporal" || entity.Entity_Type == "Increment Temporal" || entity.Entity_Type == "Add Temporal"){
                  
                  if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Temporal Buffer removing entity.Entity_Type: " + entity.Entity_Type + ", entity.Tag: " + entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
                  abortEntityTags2.addLast(entity.Tag);
                  
                }
              }
              for(Integer entityTag : abortEntityTags2){
                sim.funs.abortEntityInServer("temporalbuffer", entityTag);
              }
              
              
            }
            // if this is a clear, kill any other existing temporal looping entity, excluding self
            if(Entity.Entity_Type.equals( "Clear Temporal")) {
              
              //all entities in "602" Temporal Module
              String localServerName = "temporalmodule";
              LinkedList<Entity> entityList1 = sim.funs.getLinkedListOfQnactrEntityInServer(localServerName);
              LinkedList<Integer> abortEntityTags = new LinkedList<Integer>();
              Iterator<Entity> entities = entityList1.iterator();
              while(entities.hasNext()){
                Entity entity = entities.next();
                
                if(entity.Tag == Entity.Tag) continue; // do not abort self.
                
                if(!entity.Entity_Type.equals( "Add Temporal")){
                  
                  if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Temporal Module removing entity.Entity_Type: " + entity.Entity_Type + ", entity.Tag: " + entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
                  abortEntityTags.addLast(entity.Tag);                  
                }
              }
              
              for(Integer entityTag : abortEntityTags){
                sim.funs.abortEntityInServer(localServerName, entityTag);
              }
              
              
              //all entities in "603" Temporal Buffer
              String localServerName2 = "temporalbuffer";
              LinkedList<Entity> entityList2 = sim.funs.getLinkedListOfQnactrEntityInServer(localServerName2);
              LinkedList<Integer> abortEntityTags2 = new LinkedList<Integer>();
              Iterator<Entity> entities2 = entityList2.iterator();
              while(entities2.hasNext()){
                Entity entity = entities2.next();
                
                if(entity.Tag == Entity.Tag) continue; // do not abort self.
                
                if(!entity.Entity_Type.equals( "Add Temporal")){
                  
                  if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage + "\n Temporal Buffer removing entity.Entity_Type: " + entity.Entity_Type + ", entity.Tag: " + entity.Tag, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
                  
                  abortEntityTags2.addLast(entity.Tag);
                }
              }
              
              for(Integer entityTag : abortEntityTags2){
                sim.funs.abortEntityInServer(localServerName2, entityTag);
              }
              
              // by Shi. Clear buffer action should not kill the following add temporal request action
              
              
             
              
            }
            
            
            //actions, moved from temporal buffer, because in QN-Java, entity to temperal module is processed before entity to temperal buffer.
            // clear buffer
            if(Entity.Entity_Type.equals( "Clear Temporal")) {
              // TODO we don't merge any current temporal buffer chunk into declarative do we?
              sim.vars.temporalBuffer.Temporal_Buffer_Chunk = new Chunk();
              sim.vars.temporalBuffer.Empty = true;
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "TEMPORAL" + "\t" + "Clear");  // note, ACT-R does not output this
              Entity.Trash = true;
            }
            if(Entity.Entity_Type.equals( "Request Temporal")) {
              // if there is no chunk, set it to 0
              if(sim.vars.temporalBuffer.Empty) {
                sim.vars.temporalBuffer.Empty = false;
                // TODO is it just the chunk? i dont't think so. i think we'll at least have to set ticks=0
                // add ticks 0 to the chunk
                sim.funs.ChunkFun__Set_Chunk_Slot_Value(Entity.Chunk, "ticks", "0");
                sim.vars.temporalBuffer.Temporal_Buffer_Chunk = Entity.Chunk;
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "TEMPORAL" + "\t" + "create-new-buffer-chunk isa "+Entity.Chunk.Chunk_Type);
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "TEMPORAL" + "\t" + "set-buffer-chunk-temporal "+Entity.Chunk.Chunk_Name);
              } else {
                System.out.println("Error! Temporal Module Ending effect has temporalBuffer not Empty when Request Temporal");
                SimSystem.abort();
              }
              Entity.To = "Temporal Module"; 
            }
            // increment buffer
            if(Entity.Entity_Type.equals( "Increment Temporal")) {
              // TODO output error if temporal is empty
              // get and increment current tick value
              int tick = Integer.parseInt(sim.vars.temporalBuffer.Temporal_Buffer_Chunk.Slot.get("ticks").toString()) + 1;  // this is how many ticks have passed.
              // set incremented value
              sim.funs.ChunkFun__Set_Chunk_Slot_Value(sim.vars.temporalBuffer.Temporal_Buffer_Chunk, "ticks", Integer.toString(tick));
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "TEMPORAL" + "\t" + "Incrementing time ticks to "+tick);
              // TODO this output should appear if :record-ticks is true
              //ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(), 3) + "\t" + "TEMPORAL" + "\t" + "MODULE-MOD-REQUEST TEMPORAL");
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "TEMPORAL" + "\t" + "MOD-BUFFER-CHUNK TEMPORAL");
              
              Entity.To = "Temporal Module"; 
            }
            // modify buffer
            if(Entity.Entity_Type.equals( "Modify Temporal")) {
              // set the value of the chunk
              sim.funs.ChunkFun__Set_Chunk_Slot_Value(sim.vars.temporalBuffer.Temporal_Buffer_Chunk, "ticks", Entity.Chunk.Slot.get("ticks").toString());
            }
            
            
            
            
            break;
            
          case Timing:
            if (Entity.Entity_Type.equals( "Request Temporal")){
              // from temporal.lisp
              // set randomized initial value if it has not been set yet
              // targeting.lisp:170
              if(sim.vars.temporalModule.Time_Start_Increment == -1) {
                sim.vars.temporalModule.Time_Start_Increment = (double) (sim.vars.temporalModule.Time_Master_Start_Increment +
                    sim.funs.ProgramUtilitiesFun__Act_R_Noise(sim.vars.temporalModule.Time_Master_Start_Increment * 5 * sim.vars.temporalModule.Time_Noise));
              }
              // targeting.lisp:191
              double tick = sim.vars.temporalModule.Time_Start_Increment +
                  sim.funs.ProgramUtilitiesFun__Act_R_Noise(sim.vars.temporalModule.Time_Noise * sim.vars.temporalModule.Time_Start_Increment);  // this is the duration of this tick.
              
              // reset time to randomized initial time
              // targeting.lisp:200
              
              Entity.Duration = tick;
              
              return tick;
            }
            if (Entity.Entity_Type.equals( "Increment Temporal")) {
              // from temporal.lisp
              
              // multiply by factor
              // in lisp, the last tick time is stored in temporal-module-tick. here it is still in Entity.Duration
              // targeting.lisp:216
              
              //Entity.Duration records the time each entity spendsin the current task or queue
              double tick = Entity.Duration * sim.vars.temporalModule.Time_Mult; // this is the duration of this tick.
              
              // add noise
              // targeting.lisp:220
              tick = tick +
                  sim.funs.ProgramUtilitiesFun__Act_R_Noise(sim.vars.temporalModule.Time_Noise * tick); // this is the duration of this tick.
              
              Entity.Duration = tick;
              
              // set time to randomized increment time
              // targeting.lisp:238
              
              System.out.println("Temporal Module Timing effect has for Increment Temporal, tick = : " + tick);
              
              return tick;
            }
            if (Entity.Entity_Type.equals( "Modify Temporal")) {
              // no time taken
              return 0.0;
            }
            if (Entity.Entity_Type.equals( "Clear Temporal")) {
              // no time taken
              return 0.0;
            }
            if (Entity.Entity_Type.equals( "Add Temporal")) {
              // no time taken
              return 0.0;
            }
            return 0.0;
            	
        }
        break;

      case triggerbuffer:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); // true ;
            
            
            
          case Beginning:
            Entity.Time_Computed = false;
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            String trigger_node_name = "Trigger Buffer";
            String trigger_loop_node_name = "Trigger Buffer Loop";
            String no_loop_next_node_name = "Matching And Selection";
            
            boolean loop;
            
            //TODO, here need to accommodate multiple HMIs
            

            if(SimSystem.clock() >= 0.51) {
              int dummy = 0; //for debug to trigger break point;
            }
            
            LinkedList<Entity>currentClockEntities = QnactrSimulation.getAllEntitiesCarriedBySimEventsAtCurrentClockNoTrash();
                        
            if (  currentClockEntities.size()  > 1 ) { //wait for potentially another trigger
              
              String tags = "";
              for(Entity anEntity : currentClockEntities){
                tags += anEntity.Tag + " ";
              }
//              System.out.println("Triggerbuffer loop mechanism, currentClockEntities.size(): " + currentClockEntities.size() + ", tags: " + tags);

//              JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
              
// TODO whether need this in QN-Java??           seems NOT   
//              if (Model.Find ("Time", SimSystem.clock()).size() - Model.Find ( e => (e.Time == SimSystem.clock()) && (e.Event.ToString() == "EvaluateQueue")  ).size() == 1){  // all other entities with the same Clock are in queues
//                loop = false;
//              }
              
//              else if( Model.Find ("Time", SimSystem.clock()).size() > Model.Find ( e => e.Event_Priority > 0 ).size() )  { // there is still at least 1 Entity event that is not in any trigger looping process, loop
              
              LinkedList<Entity> eventPriorityLargerThanZeroEntities = new LinkedList<Entity>();
              for(Entity anEntity : QnactrSimulation.getNotEndedGlobalAllEntitiesList()){
                if(anEntity.Event_Priority > 0)eventPriorityLargerThanZeroEntities.addLast(anEntity);
              }
              
              if(currentClockEntities.size() > eventPriorityLargerThanZeroEntities.size()){
                loop = true;
              }

              else if(currentClockEntities.size() == eventPriorityLargerThanZeroEntities.size()) {
                //determine if this trigger has the lowest priority number (highest priority)among all the Find events
                
                int min_priority_num = 2147483647;
                for (Entity temp_entity : eventPriorityLargerThanZeroEntities ){
                  if (temp_entity.Event_Priority < min_priority_num) min_priority_num = temp_entity.Event_Priority;
                }
                // if this node has the smallest Event_Priority among all the Find events,  no loop; else loop
                if ( Entity.Event_Priority == min_priority_num ) loop = false;
                else loop = true;
              }
              else {
                System.out.println("Error! " + trigger_node_name + " has undefined find Event_Priority result.");
                break;
              }
              
            }
            else if (  currentClockEntities.size()  == 1 ) { //no loop
              loop = false;
              
            }
            else {
              System.out.println("Error! " + trigger_node_name + " has undefined case.");
              break;

            }
            
            if (loop){
              Entity.From = trigger_node_name;
              Entity.To = trigger_loop_node_name;
            }
            else{ // no loop
              Entity.From = trigger_node_name;
              Entity.To = no_loop_next_node_name;
              Entity.Event_Priority = 0; //exit Event_Priority track
            }
            
//            System.out.println(SimSystem.clock() + "  Triggerbuffer loop mechanism, loop: " + loop);
            
            
            
//            C# version backup
//            if (  Model.Find ("Time", SimSystem.clock()).size()  > 1 ){ //wait for potentially another trigger
//              //foreach( Entity an_entity in Model.Find ("Time", Clock) ){
//              //	Model.PrintOutput ("Trigger Buffer " + an_entity.ID  + " " + an_entity.Event + " " + an_entity.Type + Model.Find ("Time", Clock).size() + " " + Model.Find ( e => e.Event.ToString() == "EvaluateQueue" ).size() );
//              //}
//              
//              if (Model.Find ("Time", SimSystem.clock()).size() - Model.Find ( e => (e.Time == SimSystem.clock()) && (e.Event.ToString() == "EvaluateQueue")  ).size() == 1){  // all other entities with the same Clock are in queues
//                loop = false;
//              }
//              else if( Model.Find ("Time", SimSystem.clock()).size() > Model.Find ( e => e.Event_Priority > 0 ).size() )	{ // there is still at least 1 Entity event that is not in any trigger looping process, loop
//                loop = true;
//              }
//              else if ( Model.Find ("Time", SimSystem.clock()).size() == Model.Find ( e => e.Event_Priority > 0 ).size() ){
//                //determine if this trigger has the lowest priority number (highest priority)among all the Find events
//                
//                int min_priority_num = 2147483647;
//                for (Entity temp_entity : Model.Find ( e => e.Event_Priority > 0 ) ){
//                  if (temp_entity.Event_Priority < min_priority_num) min_priority_num = temp_entity.Event_Priority;
//                }
//                // if this node has the smallest Event_Priority among all the Find events,  no loop; else loop
//                if ( Entity.Event_Priority == min_priority_num ) loop = false;
//                else loop = true;
//              }
//              else {
//                System.out.println("Error! " + trigger_node_name + " has undefined find Event_Priority result.");
//                break;
//                loop = true; // to make the compiler happy
//              }
//              
//            }
//            else if (  Model.Find ("Time", SimSystem.clock()).size()  == 1 ) { //no loop
//              loop = false;
//              
//            }
//            else {
//              System.out.println("Error! " + trigger_node_name + " has undefined find result.");
//              break;
//              loop = true; // to make the compiler happy
//            }
//            
//            if (loop){
//              Entity.From = trigger_node_name;
//              Entity.To = trigger_loop_node_name;
//            }
//            else{ // no loop
//              Entity.From = trigger_node_name;
//              Entity.To = no_loop_next_node_name;
//              Entity.Event_Priority = 0; //exit Event_Priority track
//            }
            
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            	
        }
        break;

        
      case triggerbufferloop:
        switch (ServiceStage){
          case Release:
            return  (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true ;
            
            
            
          case Beginning:
            
            // no beginning effect
            
            break;
            
          case Ending:
            boolean debug = false;
            
            if(serverLogicsDebugPopupFlag && debug)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            
            Entity.From = "Trigger Buffer Loop" ;
            Entity.To = "Trigger Buffer";
       
            if ( ( sim.funs.getNumberOfQnactrEntityInServerAndQueue("triggerbuffer")  + sim.funs.getNumberOfQnactrEntityInServerAndQueue("triggerbufferloop")) > 1 ){ //more than one trigger
              Entity.Trash = true;
              if(debug)System.out.println("Triggerbufferloop loop mechanism. Trashed Entity tag: " + Entity.Tag);
            }
            else {
              
              //here may need to find entites that is going to the trigger buffer loop from the trigger buffer
              boolean breakflag = false;
              LinkedList<Entity>currentClockEntities = QnactrSimulation.getAllEntitiesCarriedBySimEventsAtCurrentClockNoTrash();
              for(Entity anEntity : currentClockEntities){
                
                if(anEntity.currentPlaceHeader == Enums.EntityPlaceHeader.goingto && anEntity.currentServerNodeLocalName == Enums.ServerName.triggerbufferloop) {
                  Entity.Trash = true;
                  if(debug)System.out.println("Triggerbufferloop loop mechanism. Trashed Entity tag (goingto trigger buffer loop): " + Entity.Tag);
                  breakflag = true;
                  break;
                }
                
              }
              
              if(breakflag == false){
                Entity.Event_Priority = (int) sim.vars.programGlobalVar__Event_Priority_Table.get(Entity.From);
                if(debug)System.out.println("Triggerbufferloop loop mechanism. Does NOT Trashed Entity tag: " + Entity.Tag);
              }
            }
            
//            C# version backup
//            if ( ( Model.Find("ID", "110").Count + Model.Find("ID", "111").Count ) > 1 ){ //more than one trigger
//              Entity.Trash = true;
//            }
//            else Entity.Event_Priority = (int) sim.vars.programGlobalVar__Event_Priority_Table.get("Trigger Buffer Loop");
//            
            
            break;
            
          case Timing:
            return 0.0;
            	
        }
        break;
        
        
      case visionmodule:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
                    
          case Beginning:
            
            Entity.Event_Priority = 0; //reset this value that may be modified in vision module trigger
            
            if(Entity.Trash == true) { //pass 
            }
            else if (Entity.From.equals( "Vision Module Trigger") && Entity.Entity_Type.equals( "Visual Display Change Notice")) { //if entity comes from visual display, first check and do visual buffer stuffing  and re-encoding.
              //currently don't have buffer stuffing for World3D objects
              if (sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk.Chunk_Name.equals( "" ) && sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk.Chunk_Type.equals( "" ) && sim.vars.visualDisplay.Visicon.size() > 0){ //only do visual-location buffer stuffing when there is something in the visicon
                //visual-location buffer stuffing
                //GlobalUtilities.popUpMessage("Vision Module buffer stuffing");
                //ACT-R: The default preference is for the left-most (screen-x lowest) unattended item (:attended nil). if more than one candidate, randomly pick one.
                Chunk temp_chunk = new Chunk();
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "screen-y", "lowest" ); //ACTR-QN adds this for transcript typing model
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "screen-x", "lowest" );
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, ":attended", "nil" ); //nil test includes new
                Chunk selected_visual_location_chunk_clone = sim.funs.VisionModuleFun__Find_Visual_Location_In_Visicons_By_Chunk_Spec (temp_chunk, true); //exclude just selected locations
                sim.vars.visionModule.Buffer_Stuffing_Just_Selected_Attended_Nil_Visual_Location_Names.addLast( selected_visual_location_chunk_clone.Chunk_Name);
                // if there was no location found that can be stuffed, then abort the entity
                if (selected_visual_location_chunk_clone.Chunk_Name.equals( "" )) {
                  // abort buffer stuffing
                  Entity.Trash = true;
                } else {
                  sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk = selected_visual_location_chunk_clone;
                  
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "SET-BUFFER-CHUNK VISUAL-LOCATION " + sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk.Chunk_Name + "  REQUESTED NIL"); 
                  sim.vars.visualLocationBuffer.Empty = false;
                  sim.vars.visualLocationBuffer.Requested = false;
                  sim.vars.visualLocationBuffer.State_Error = false; 
                  sim.vars.visualLocationBuffer.State_Free = true;
                  sim.vars.visualLocationBuffer.Unrequested = true;
                  
                  //keep the type as "Visual Display Change Notice" to trigger re-encoding check
                  //and make a new entity for production conflict resolution trigger:
                  if( sim.vars.visionModule.Trigger_Procedural_Conflict_Resolution_After_Visual_Location_Buffer_Stuffing  ){
                    
                    sim.funs.createEntity( "Visual Location Buffer" , "Vision Module", "Visual-location Buffer", "Production Rule Firing Trigger", 0.0);
                    //                    Entity Temp_Entity = new Entity();  
                    //                    Temp_Entity.ID = "12"; //Visual Location buffer
                    //                    Temp_Entity.Time = (double) SimSystem.clock();
                    //                    Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
                    //                    QnactrSimulation.entityNumber++;
                    //                    Temp_Entity.From = "Vision Module"; //tag from and to
                    //                    Temp_Entity.To = "Visual-location Buffer";	//to Visual-location Buffer rather than visual buffer because it is Visual-location Buffer stuffing
                    //                    Temp_Entity.Entity_Type = "Production Rule Firing Trigger"	;
               
                  }
                }
              }
              
              //when to trigger re-encoding check?
              //when an entity arrives at the vision module, representing some change on the visual display
              if (sim.funs.VisionModuleFun__Is_Re_Encoding()) { 
                // don't do re-encode if module already busy
                if(!sim.vars.visionModule.Moving_Attention) {
                  if (sim.vars.visionModule.Reencoding_Makes_Vision_Busy)	{
                    if(sim.vars.visionModule.State_Free) {
                      sim.vars.visionModule.State_Free = false;
                      Entity.Note = "reencode";
                    } else {
                      // kill the update because we're already in the middle of an encode
                      Entity.Trash = true;
                    }
                  }
                }
              }
            }
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Visual" )){
              if (!Entity.Chunk.Chunk_Type.equals( "clear")){
                //GlobalUtilities.popUpMessage("Vision Module add visual chunk type: " + Entity.Chunk.Chunk_Type);		
                if(Entity.Chunk.Chunk_Type.equals( "move-attention")){		
                  //Entity temp_entity = new Entity();
                  //temp_entity.Chunk = sim.funs.VisionModuleFun__Find_Visual_Display_Chunk_By_Chunk_Location_Spec(Entity.Chunk);
                  //temp_entity.Chunk = sim.funs.VisionModuleFun__Find_Visicon_By_Chunk_Spec(Entity.Chunk);
                  String visual_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "screen-pos");
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + Entity.Chunk.Chunk_Type + " " + visual_location_chunk_name + " NIL"); 
                  sim.vars.visionModule.State_Free = false;
                  sim.vars.visionModule.Moving_Attention = true;
                  
                  //Animator or Animator3D show visual attention location
//                  System.out.println("TODO visionmodule, Animator or Animator3D show visual attention location");

                  if(   (visual_location_chunk_name.length() >= 10 && visual_location_chunk_name.substring(0,10).equals( "near-point")) ) {
                    if(QnactrSimulation.taskVisualization2DEnable)sim.vars.taskVisualization2D.hideObject(sim.vars.taskVisualization2D.visualAttentionCircleID); //Animator.HideComment("500");
                    World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                    System.out.println("ToDo, vision module Animator3D show visual attention location on near-point");
                    method_pointer.Visual_Attention_Location_World3D_ID = visual_location_chunk_name.substring(11); // "near-point-"
                    
                  }
                  else if ( (visual_location_chunk_name.length() >= 9 && visual_location_chunk_name.substring(0,9).equals( "far-point")) ){ 		
                    if(QnactrSimulation.taskVisualization2DEnable)sim.vars.taskVisualization2D.hideObject(sim.vars.taskVisualization2D.visualAttentionCircleID); //Animator.HideComment("500");
                    World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                    method_pointer.Visual_Attention_Location_World3D_ID = visual_location_chunk_name.substring(10); // "far-point-";
                    //GlobalUtilities.popUpMessage(method_pointer.Visual_Attention_Location_World3D_ID);
                  }
                  else if ( (visual_location_chunk_name.length() >= 16 && visual_location_chunk_name.substring(0,16).equals( "customized-point")) ){ 		
                    if(QnactrSimulation.taskVisualization2DEnable)sim.vars.taskVisualization2D.hideObject(sim.vars.taskVisualization2D.visualAttentionCircleID); //Animator.HideComment("500");
                    World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                    method_pointer.Visual_Attention_Location_World3D_ID = visual_location_chunk_name.substring(17); // "customized-point-";
                  }
                  else if((visual_location_chunk_name.length() >= 16 && visual_location_chunk_name.substring(0,16).equals( "critical-element"))) {
                	  // critical element
                	  //form visual chunk, with kind of world3d-driving-criticalelement-vehicle, or world3d-driving-criticalelement-sign
                      World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                      method_pointer.Visual_Attention_Location_World3D_ID = visual_location_chunk_name.substring(17); // "critical-element-"
                  }
                  //end of Animator 3D
                  else{ // Animator 2D
                    if( sim.vars.world3DTemplate.Method_Object != null && (sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method)) {
                      World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                      method_pointer.Visual_Attention_Location_World3D_ID = ""; 
                    }
                    
                    if(sim.vars.animatorModule.Show_Animator && sim.vars.animatorModule.Show_Visual_Attention_Focus){ 
                      //if considering the 85 ms visual delay is for eye movement and encoding, then here is the start of the eye movement. Since animator2D has difficulty to show the movement during the period, here move the visual attention icon at the beginning of the movement.
                      Chunk visual_location_chunk = (Chunk) sim.vars.centralParametersModule.Chunks.get(visual_location_chunk_name);
                      double x = sim.vars.animatorModule.Visual_Attention_Offset_Screen_X + Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(visual_location_chunk, "screen-x") ) ;
                      double y = sim.vars.animatorModule.Visual_Attention_Offset_Screen_Y + Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value(visual_location_chunk, "screen-y") ) ;
                      if(QnactrSimulation.taskVisualization2DEnable){
                        sim.vars.taskVisualization2D.showObject(sim.vars.taskVisualization2D.visualAttentionCircleID); //Animator.ShowComment("500"); Animator.MoveCommentToTop ("500");
                        sim.vars.taskVisualization2D.setDynamicObjectLocation(sim.vars.taskVisualization2D.visualAttentionCircleID, (int)x, (int)y); // Animator.MoveComment ("500", x , y  );
                      }
                      
                    }
                  }
                  
                }
                else System.out.println("Vision Module Beginning Effect has an unknown chunk type: " + Entity.Chunk.Chunk_Type);
              }
              else { //+visual> isa clear
                // the clear buffer work is done byClear_Visual_Buffer_Request  called inAdd_Visual_Request
                
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "CLEAR");
                sim.vars.visionModule.State_Preparation_Free = false;
              }
            }
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Visual-location")){ //pass
            }
            else System.out.println( "Vision Module beginning effect has an undefined case, Entity.From: " + Entity.From + ", Entity.Entity_Type: " + Entity.Entity_Type);
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Visicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Location_Buffer_Contents();
            
            
            break;
            
          case Ending:
            
            //GlobalUtilities.popUpMessage(Entity.Entity_Type);
            
            //            	if (Entity.From.equals( "Vision Module Trigger") && Entity.Entity_Type.equals( "Visual-location Buffer Stuffing Notice")){
            //            		Entity.From = "Vision Module";
            //            		Entity.To = "Visual-location Buffer";
            //            	}
            
            if(Entity.Trash == true) { //pass 
            }
            else if (Entity.From.equals( "Vision Module Trigger") &&  Entity.Entity_Type.equals( "Visual Display Change Notice")){	
              //when to trigger re-encoding check?
              //when an entity arrives at the vision module, representing some change on the visual display
              // check for reencode note in entity instead of evaluating reencode function again because visual display may have changed but we should still finish reencode
              if(Entity.Note.equals( "reencode")) {
                //Re-encoding 
                //how to execute re-encoding?
                //ACTR-QN implementation: 
                //1.	re-encode try to find and only find the visicon that is at the same screen location of the last attended visicon and with the same visicon type
                
                if(sim.vars.visualDisplay.Visicon.size() > 0 && !sim.vars.visionModule.Last_Attended_Visicon_Name.equals( "" )){
                  Chunk temp_chunk = new Chunk();
                  temp_chunk.Chunk_Type = ""; //this means matching any chunk type, which can be ignored because "" is the default chunk type for a new chunk
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "screen-x", sim.vars.visionModule.Last_Attended_Screen_X);
                  sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "screen-y", sim.vars.visionModule.Last_Attended_Screen_Y);
                  String last_attended_chunk_type = ( (Chunk) sim.vars.centralParametersModule.Chunks.get(sim.vars.visionModule.Last_Attended_Visicon_Name)).Chunk_Type;
                  //GlobalUtilities.popUpMessage("Vision module last_attended_chunk_type: " + last_attended_chunk_type );
                  temp_chunk.Chunk_Type = last_attended_chunk_type;
                  Entity.Chunk = sim.funs.VisionModuleFun__Find_Visicon_By_Location(temp_chunk);
                } else {
                  Entity.Chunk = new Chunk();
                }
                
                if ( sim.vars.visualDisplay.Visicon.size() <= 0  || (Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" ) ) ){
                  if( sim.vars.visionModule.State_Error == false){ // to avoid showing this multiple time when multiple items disappearing at the same time.
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " +  sim.vars.visionModule.Last_Attended_Visual_Location_Name + " NIL");
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "No visual-object found at screen-x-y: (" + sim.vars.visionModule.Last_Attended_Screen_X + ", " + sim.vars.visionModule.Last_Attended_Screen_Y + ")"); 
                    sim.vars.visionModule.State_Error = true;
                    
                    sim.funs.VisionModuleFun__Clear_Last_Attended_Info();
                    
                    if (sim.vars.messageOn) System.out.println ("Vision_Module retrieval failed.");
                    
                    Entity.From = "Vision Module";
                    Entity.To = "Visual Buffer";	
                    Entity.Entity_Type = "Production Rule Firing Trigger"	;
                  }
                } //failed
                else{
                  if (sim.vars.messageOn) System.out.println ("Vision_Module retrieval succeeded");
                  String visual_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "screen-pos");
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " + visual_location_chunk_name + " NIL"); 
                  sim.funs.VisionModuleFun__Place_Visual_Finst_On (Entity.Chunk.Chunk_Name);
                  sim.vars.visionModule.State_Error = false;
                  if (sim.vars.messageOn) System.out.println ("Vision_Module retrieval succeeded");
                  
                //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
                  //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
                  //QnactrSimulation.entityNumber++;
                  Entity.From = "Vision Module"; //tag from and to
                  Entity.To = "Visual Buffer";
                  Entity.Entity_Type = "Visual Buffer's New Chunk";
                }
                sim.vars.visionModule.State_Free = true;
                
                //GlobalUtilities.popUpMessage("Vision Module ending effect: re-encoding end");
              }
              else {
                //no re-encoding
                //GlobalUtilities.popUpMessage("!");
                Entity.From = "Vision Module";
                Entity.To = "Visual Buffer";	
                Entity.Entity_Type = "Production Rule Firing Trigger"	;
                
                int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
                if(itemsInTask > 1){
                  //GlobalUtilities.popUpMessage("Vision Module itemsInTask:" + itemsInTask);
                  //do not set state free to false, because there is another entity still in this module.
                }
                else sim.vars.visionModule.State_Free = true;
              }
            }
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Visual-location")){
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
              //QnactrSimulation.entityNumber++;
              Entity.From = "Vision Module"; //tag from and to
              Entity.To = "Visual-location Buffer";
              Entity.Entity_Type = "Visual-location Buffer's New Chunk";    
              String visual_location_type = Entity.Chunk.Chunk_Type;
              //GlobalUtilities.popUpMessage( Entity.Chunk.Chunk_Name + " " + Entity.Chunk.Chunk_Type);
              if(visual_location_type.equals( "visual-location")){
                Entity.Chunk = sim.funs.VisionModuleFun__Find_Visual_Location_In_Visicons_By_Chunk_Spec(Entity.Chunk, false); //find the visual-location chunk in visual display
              }
              else if (visual_location_type.equals( "visual-location-world3d-driving")){
                Entity.Chunk = sim.funs.VisionModuleFun__Find_Visual_Location_In_World3D_By_Chunk_Spec(Entity.Chunk);
                //sim.vars.visualDisplay.World3D_Visual_Locations.put( Entity.Chunk.Chunk_Name, sim.funs.ChunkFun__Chunk_Clone( Entity.Chunk ) );
              }
              else if (visual_location_type.equals( "visual-location-world3d-driving-criticalelement")){
                  Entity.Chunk = sim.funs.VisionModuleFun__Find_Visual_Location_In_World3D_By_Chunk_Spec(Entity.Chunk);
                  //sim.vars.visualDisplay.World3D_Visual_Locations.put( Entity.Chunk.Chunk_Name, sim.funs.ChunkFun__Chunk_Clone( Entity.Chunk ) );
                }
              else {
                System.out.println("Error! Vision Module has undefined Add visual_location_type: " + visual_location_type);
                break;
              }
              
              if (Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" )){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Find-location");
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "FIND-LOC-FAILURE");
                sim.vars.visualLocationBuffer.Empty = true;
                sim.vars.visualLocationBuffer.Requested = false;
                sim.vars.visualLocationBuffer.State_Error = true;
                sim.vars.visualLocationBuffer.State_Free = true;
                sim.vars.visualLocationBuffer.Unrequested = false;
                //System.out.println ("Visual-location retrieval failed.");
              } //failed
              else{
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Find-location"); 
                sim.vars.visualLocationBuffer.Empty = false;
                sim.vars.visualLocationBuffer.Requested = true;
                sim.vars.visualLocationBuffer.State_Error = false;
                sim.vars.visualLocationBuffer.State_Free = true;
                sim.vars.visualLocationBuffer.Unrequested = false;
              }
            }
            else if (Entity.From.equals( "Execution" ) && Entity.Entity_Type.equals( "Add Visual" )) { 
              if (!Entity.Chunk.Chunk_Type.equals( "clear")){
              //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
                //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
                //QnactrSimulation.entityNumber++;
                Entity.From = "Vision Module"; //tag from and to
                Entity.To = "Visual Buffer";
                Entity.Entity_Type = "Visual Buffer's New Chunk";    

                //Entity.Chunk = sim.funs.VisionModuleFun__Find_Visual_Display_Chunk_By_Chunk_Location_Spec(Entity.Chunk); //find the chunk
                //Entity.Chunk = sim.funs.VisionModuleFun__Find_Visicon_By_Chunk_Spec(Entity.Chunk); //find the chunk
                String visual_location_chunk_name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "screen-pos");
                //Chunk visual_chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
                Chunk temp_chunk = new Chunk();
                sim.funs.ChunkFun__Add_Chunk_Slot_Name_And_Value(temp_chunk, "screen-pos", visual_location_chunk_name);
                
                //world3d visual object special cases
                if(   (visual_location_chunk_name.length() >= 11 && visual_location_chunk_name.substring(0,11).equals( "near-point-")) ) {
                  temp_chunk.Chunk_Type = "near-point";
                }
                else if ( (visual_location_chunk_name.length() >= 10 && visual_location_chunk_name.substring(0,10).equals( "far-point-")) ){ 
                  temp_chunk.Chunk_Type = "far-point";
                }
                else if ( visual_location_chunk_name.equals( "customized-point-dummy-visual-location-point-vehicle-a" ) ){
                  temp_chunk.Chunk_Type = "vehicle-a";
                }
                else if ( (visual_location_chunk_name.length() >= 17 && visual_location_chunk_name.substring(0,17).equals( "critical-element-")) ){
                   temp_chunk.Chunk_Type = "critical-element";
                }
                
                
                Entity.Chunk = sim.funs.VisionModuleFun__Find_Visicon_By_Location((Chunk)sim.vars.centralParametersModule.Chunks.get((String)temp_chunk.Slot.get("screen-pos")));
                
                /*
            			if(   (visual_location_chunk_name.Length >= 10 && visual_location_chunk_name.Substring(0,10).equals( "near-point")) ) {
            				visual_chunk.Chunk_Name = "near-point";
            				Entity.Chunk = visual_chunk;
            				ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(), 3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " + visual_location_chunk_name  + " NIL"); 
            				//VisionModuleFun__Place_Visual_Finst_On (Entity.Chunk.Chunk_Name);
            				sim.vars.visionModule.State_Error = false;
            			}
            			else if ( (visual_location_chunk_name.Length >= 9 && visual_location_chunk_name.Substring(0,9).equals( "far-point")) ){ 		

            				//GlobalUtilities.popUpMessage(ChunkFun__Get_Chunk_Contents(visual_location_chunk));
            				visual_chunk.Chunk_Name = "far-point";
            				Entity.Chunk = visual_chunk;
            				ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(), 3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " + visual_location_chunk_name  + " NIL"); 
            				//VisionModuleFun__Place_Visual_Finst_On (Entity.Chunk.Chunk_Name);
            				sim.vars.visionModule.State_Error = false;
            			}
            			
            			else 
                 */	
                if (Entity.Chunk.Chunk_Name.equals( "" ) && Entity.Chunk.Chunk_Type.equals( "" )){
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " + visual_location_chunk_name  + " NIL"); 
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "No visual-object found");
                  Entity.From = "Vision Module";
                  Entity.To = "Visual Buffer";	
                  Entity.Entity_Type = "Production Rule Firing Trigger"	;
                  //System.out.println ("add Visual, Vision_Module retrieval failed.");
                  if(temp_chunk.Chunk_Type.equals("critical-element")) sim.vars.visionModule.State_Error = true;
                } //failed
                else{
                  if (sim.vars.messageOn) System.out.println ("add Visual, Vision_Module retrieval succeeded");
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "Encoding-complete " + visual_location_chunk_name  + " NIL"); 
                  if( Entity.Chunk.Chunk_Name.length() >= 7 && Entity.Chunk.Chunk_Name.substring(0,7).equals( "world3d")){
                    //Currently don't have visual finst for world3d objects.
                  }
                  else{
                    sim.funs.VisionModuleFun__Place_Visual_Finst_On (Entity.Chunk.Chunk_Name);
                  }
                  
                  sim.vars.visionModule.State_Error = false;
                }
                sim.vars.visionModule.State_Free = true;
                sim.vars.visionModule.Moving_Attention = false;
              }
              else { //+visual> isa clear, only this can disable re-encoding by clear the last attended information
                
                sim.funs.VisionModuleFun__Clear_Last_Attended_Info();
                /*
            			sim.vars.visionModule.Last_Attended_Screen_X = "";
            			sim.vars.visionModule.Last_Attended_Screen_Y = "";
            			sim.vars.visionModule.Last_Attended_Visicon_Name = "";
            			sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
            			sim.vars.visionModule.Last_Attended_Visual_Location_Name = "";
                 */ //replaced by sim.funs.VisionModuleFun__Clear_Last_Attended_Info();
                
                
                sim.vars.visionModule.State_Error = false;
                sim.vars.visionModule.State_Preparation_Free = true;
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "CHANGE-STATE LAST NONE PREP FREE");
                
              //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
                //Entity.Tag = QnactrSimulation.entityNumber; //give it an new entity number, init. 1
                //QnactrSimulation.entityNumber++;
                Entity.From = "Vision Module"; //tag from and to
                Entity.To = "Visual Buffer";
                Entity.Entity_Type = "Visual Clear Notice";    
                Entity.Chunk = new Chunk(); 
              }
            }
            
            else System.out.println( "Vision Module ending effect has an undefined case");
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Visicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Location_Buffer_Contents();
            
            /* //no need because "Visual-location Buffer Stuffing" no longer exist
            	if (Entity.From.equals( "Vision Module Trigger") && Entity.Entity_Type.equals( "Visual-location Buffer Stuffing")){
            		Entity.From = "Vision Module";
            		Entity.To = "Visual-location Buffer";	//to Visual-location Buffer rather than visual buffer because it is Visual-location Buffer stuffing
            		Entity.Entity_Type = "Production Rule Firing Trigger"	;
            	}
             */
            
            sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.utilization__Vision_Module_Changes_In_A_Second, (double)GlobalUtilities.round (SimSystem.clock(),3) , 0.0 ) ; 
            
            break;
            
          case Timing:

            double time;
            
            if(Entity.Trash == true) time = 0.0;
            else if (Entity.From.equals( "Execution") && Entity.Entity_Type.equals( "Add Visual")){
              if (!Entity.Chunk.Chunk_Type.equals( "clear"))	{
                //no learning visual attention latency 
                time = sim.vars.visionModule.Visual_Attention_Latency; 
                
                /*
            			//QN learning of visual attention latency
            			double A = sim.vars.visionModule.Visual_Attention_Latency * 0.50;
            			double B = sim.vars.visionModule.Visual_Attention_Latency * 1.50; //Feyer 2002, perceptual time, slow 200ms, typical 100ms, fast 50ms
            			double alpha = 0.001;
            			double N = sim.vars.motorModule.Typing_Learning_N_Total;
            			time = A + B * Math.Exp( -1.0 * alpha * N);
                 */
              }
              else time = 0.050; //ACT-R reference: A clear request will make the preparation state busy for 50ms. 
            }
            else if (Entity.Entity_Type.equals( "Visual Display Change Notice") && sim.funs.VisionModuleFun__Is_Re_Encoding() ) {
              //obsolete //else if (Entity.Entity_Type.equals( "Visual Display Change Notice") && !sim.vars.visionModule.Last_Attended_Screen_X.equals("") && !sim.vars.visionModule.Last_Attended_Screen_Y.equals("")) {
              time = sim.vars.visionModule.Visual_Attention_Latency;  //reencoding
            }
            else time = 0.0;
            
            Entity.Time_Computed = true;
            
            if(time > 0.0)  sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value(sim.vars.utilization__Vision_Module_Changes_In_A_Second, (double)GlobalUtilities.round (SimSystem.clock(),3) , 1.0 ) ; 
            
            return time;
            	
        }
        break;
        
        
      case visionmoduletrigger:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
            
          case Beginning:
            
            // no beginning
            
            break;
            
          case Ending:

            String trigger_node_name = "Vision Module Trigger";
            String trigger_loop_node_name = "Vision Module Trigger Loop";
            String no_loop_next_node_name = "Vision Module";
            
            boolean loop;
 
            
            //TODO, here need to accommodate multiple HMIs
            
            LinkedList<Entity>currentClockEntities = QnactrSimulation.getAllEntitiesCarriedBySimEventsAtCurrentClockNoTrash();
            if (  currentClockEntities.size()  > 1 ) { //wait for potentially another trigger
              
              String tags = "";
              for(Entity anEntity : currentClockEntities){
                tags += anEntity.Tag + " ";
              }
              
              //System.out.println("Visionmoduletrigger loop mechanism, currentClockEntities.size(): " + currentClockEntities.size() + ", tags: " + tags);

//              JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
              
// TODO whether need this in QN-Java??           seems NOT   
//              if (Model.Find ("Time", SimSystem.clock()).size() - Model.Find ( e => (e.Time == SimSystem.clock()) && (e.Event.ToString() == "EvaluateQueue")  ).size() == 1){  // all other entities with the same Clock are in queues
//                loop = false;
//              }
              
//              else if( Model.Find ("Time", SimSystem.clock()).size() > Model.Find ( e => e.Event_Priority > 0 ).size() )  { // there is still at least 1 Entity event that is not in any trigger looping process, loop
              
              LinkedList<Entity> eventPriorityLargerThanZeroEntities = new LinkedList<Entity>();
              for(Entity anEntity : QnactrSimulation.getNotEndedGlobalAllEntitiesList()){
                if(anEntity.Event_Priority > 0)eventPriorityLargerThanZeroEntities.addLast(anEntity);
              }
              
              if(currentClockEntities.size() > eventPriorityLargerThanZeroEntities.size()){
                loop = true;
              }

              else if(currentClockEntities.size() == eventPriorityLargerThanZeroEntities.size()) {
                //determine if this trigger has the lowest priority number (highest priority)among all the Find events
                
                int min_priority_num = 2147483647;
                for (Entity temp_entity : eventPriorityLargerThanZeroEntities ){
                  if (temp_entity.Event_Priority < min_priority_num) min_priority_num = temp_entity.Event_Priority;
                }
                // if this node has the smallest Event_Priority among all the Find events,  no loop; else loop
                if ( Entity.Event_Priority == min_priority_num ) loop = false;
                else loop = true;
              }
              else {
                System.out.println("Error! " + trigger_node_name + " has undefined find Event_Priority result.");
                break;
              }
              
            }
            else if (  currentClockEntities.size()  == 1 ) { //no loop
              loop = false;
              
            }
            else {
              System.out.println("Error! " + trigger_node_name + " has undefined case.");
              break;

            }
            
            if (loop){
              Entity.From = trigger_node_name;
              Entity.To = trigger_loop_node_name;
            }
            else{ // no loop
              Entity.From = trigger_node_name;
              Entity.To = no_loop_next_node_name;
              Entity.Event_Priority = 0; //exit Event_Priority track
            }
            
            
            
//            c# version
//            if (  Model.Find ("Time", SimSystem.clock()).size()  > 1 ){ //wait for potentially another trigger
//              if( Model.Find ("Time", SimSystem.clock()).size() > Model.Find ( e => e.Event_Priority > 0 ).size() )	{ // there is still at least 1 Entity event that is not in any trigger looping process, loop
//                loop = true;
//              }
//              else if ( Model.Find ("Time", SimSystem.clock()).size() == Model.Find ( e => e.Event_Priority > 0 ).size() ){
//                //determine if this trigger has the lowest priority number (highest priority)among all the Find events
//                
//                int min_priority_num = 2147483647;
//                for (Entity temp_entity : Model.Find ( e => e.Event_Priority > 0 ) ){
//                  if (temp_entity.Event_Priority < min_priority_num) min_priority_num = temp_entity.Event_Priority;
//                }
//                // if this node has the smallest Event_Priority among all the Find events,  no loop; else loop
//                if ( Entity.Event_Priority == min_priority_num ) loop = false;
//                else loop = true;
//              }
//              else {
//                System.out.println("Error! " + trigger_node_name + " has undefined find Event_Priority result.");
//                SimSystem.abort();
//                loop = true; // to make the compiler happy
//              }
//              
//            }
//            else if (  Model.Find ("Time", SimSystem.clock()).size()  == 1 ) { //no loop
//              loop = false;
//              
//            }
//            else {
//              System.out.println("Error! " + trigger_node_name + " has undefined find result.");
//              SimSystem.abort();
//              loop = true; // to make the compiler happy
//            }
//            
//            if (loop){
//              Entity.From = trigger_node_name;
//              Entity.To = trigger_loop_node_name;
//            }
//            else{ // no loop
//              Entity.From = trigger_node_name;
//              Entity.To = no_loop_next_node_name;
//              Entity.Event_Priority = 0; //exit Event_Priority track
//            }
//            
            break;
            
          case Timing:
            return 0.0;
            	
        }
        break;
        
        
      case visionmoduletriggerloop:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
            
          case Beginning:
            
            // no beginning
            
            break;
            
          case Ending:
            
  
            Entity.From = "Vision Module Trigger Loop" ;         
            Entity.To = "Vision Module Trigger";
                   
            if ( ( sim.funs.getNumberOfQnactrEntityInServerAndQueue("visionmoduletrigger")  + sim.funs.getNumberOfQnactrEntityInServerAndQueue("visionmoduletriggerloop")) > 1 ){ //more than one trigger
              Entity.Trash = true;
//              System.out.println("visionmoduletriggerloop loop mechanism. Trashed Entity tag: " + Entity.Tag);
            }
            else {
              
              //here may need to find entites that is going to the trigger buffer loop from the trigger buffer
              boolean breakflag = false;
              LinkedList<Entity>currentClockEntities = QnactrSimulation.getAllEntitiesCarriedBySimEventsAtCurrentClockNoTrash();
              for(Entity anEntity : currentClockEntities){
                
                if(anEntity.currentPlaceHeader == Enums.EntityPlaceHeader.goingto && anEntity.currentServerNodeLocalName == Enums.ServerName.visionmoduletriggerloop) {
                  Entity.Trash = true;
//                  System.out.println("visionmoduletriggerloop loop mechanism. Trashed Entity tag (goingto trigger buffer loop): " + Entity.Tag);
                  breakflag = true;
                  break;
                }
                
              }
              
              if(breakflag == false){
                Entity.Event_Priority = (int) sim.vars.programGlobalVar__Event_Priority_Table.get(Entity.From);
//                System.out.println("visionmoduletriggerloop loop mechanism. Does NOT Trashed Entity tag: " + Entity.Tag);
              }
            }
            
//            c# versin
//            if ( ( Model.Find("ID", "112").size() + Model.Find("ID", "113").size() ) > 1 ){ //more than one trigger
//              Entity.Trash = true;
//            }
//            else {
//              //GlobalUtilities.popUpMessage( sim.funs.ProgramUtilitiesFun__LinkedListString_To_String_Show_Empty( sim.funs.ProgramUtilitiesFun__Hashtable_To_LinkedListString(sim.vars.programGlobalVar__Event_Priority_Table, "keys")) );
//              Entity.Event_Priority = (int) sim.vars.programGlobalVar__Event_Priority_Table.get("Vision Module Trigger Loop");
//            }
//            
            break;
            
          case Timing:
            return 0.0;
            	
        }
        break;
        
        
      case visuallocationbuffer:
        switch (ServiceStage){
          case Release:
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
          case Beginning:
            
            Entity.Time_Computed = false;
            if (Entity.Entity_Type.equals( "Visual-location Buffer's New Chunk")) {
              /* //since visual-location buffer chunk will not be modified, it seems not necessary to rename it.
        			//put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
        			int j = 0;
        			string old_chunk_name = Entity.Chunk.Chunk_Name;
        			string new_chunk_name = old_chunk_name + "-" + j.ToString();
        			while(ChunkFun__Is_Chunk_Name(new_chunk_name)){
        				j++;
        				new_chunk_name = old_chunk_name + "-" + j.ToString();
        			}
        			Entity.Chunk.Chunk_Name = new_chunk_name;
        			if(DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
        				Entity.Chunk.Activation = 0.0; //all default values
        				Entity.Chunk.Creation_Time = 0.0;
        				Entity.Chunk.Number_Of_Presentations = 0;
        				Entity.Chunk.Presentation_Time_References  = new LinkedList<double> ();
        			}
        			ChunkFun__Define_Chunk( Entity.Chunk );
               */
    			
              sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              if(!Entity.Chunk.Chunk_Name.equals( "" ) && !Entity.Chunk.Chunk_Type.equals( "" )){ //verifed with ACT-R command (buffer-chunk)
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "SET-BUFFER-CHUNK VISUAL-LOCATION " + sim.vars.visualLocationBuffer.Visual_Location_Buffer_Chunk.Chunk_Name  ); 
                //ProgramUtilitiesFun__Output_Trace_Txt( sim.funs.ChunkFun__Get_Chunk_Contents (Entity.Chunk) );
            	// added by Yelly
            	// I need visual-location chunk to be in the center chunks,
    			sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
              }
            }
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Visicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Location_Buffer_Contents();
            
            
            // Ending Effect
            if (Entity.Entity_Type.equals( "Visual-location Buffer's New Chunk" )) { //|| Entity.Entity_Type.equals( "Production Rule Firing Trigger") //trigger moves to visual buffer
              Entity.From = "Visual-location Buffer"; //change the entity and pass it to Matching And Selection as a trigger
              Entity.To = "Matching And Selection";
              Entity.Entity_Type = "Production Rule Firing Trigger";
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; 
              //QnactrSimulation.entityNumber++;

            }
            
            
            if (Entity.Entity_Type.equals( "Production Rule Firing Trigger")){
              Entity.From = "Visual-location Buffer";
              Entity.To = "Matching And Selection";	
            }
            
            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            break;
            
          case Ending:
            
            if (Entity.Entity_Type.equals( "Visual-location Buffer's New Chunk") ) { //|| Entity.Entity_Type.equals( "Production Rule Firing Trigger") //trigger moves to visual buffer
              Entity.From = "Visual-location Buffer"; //change the entity and pass it to Matching And Selection as a trigger
              Entity.To = "Matching And Selection";
              Entity.Entity_Type = "Production Rule Firing Trigger";
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; 
              //QnactrSimulation.entityNumber++;

            }
            
            
            if (Entity.Entity_Type.equals( "Production Rule Firing Trigger")){
              Entity.From = "Visual-location Buffer";
              Entity.To = "Matching And Selection";	
            }
            
            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            	
        }
        break;
        
        
      case visualbuffer:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            
            
            
          case Beginning:

            Entity.Time_Computed = false;
            if (Entity.Entity_Type.equals( "Visual Buffer's New Chunk")) {
              //put Chunk in buffer naming rule: its name is changed into name-j, where j starts from 0. If "name-j" is already a name in the model chunk list, then j++, unitl it is a new name.
              int j = 0;
              String old_chunk_name = Entity.Chunk.Chunk_Name;
              String new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              while(sim.funs.ChunkFun__Is_Chunk_Name(new_chunk_name)){
                j++;
                new_chunk_name = old_chunk_name + "-" + Integer.toString(j);
              }
              Entity.Chunk.Chunk_Name = new_chunk_name;
              if(sim.funs.DeclarativeModuleFun__Find_The_DM_Chunk_ID_By_Chunk_Name(old_chunk_name) != -1) {	//when the old chunk is already in DM, need to clear the DM parameters in the old chunk for the new chunk
                Entity.Chunk.Activation = (double) 0.0; //all default values
                Entity.Chunk.Creation_Time = 0.0;
                Entity.Chunk.Number_Of_Presentations = 0;
                Entity.Chunk.Presentation_Time_References  = new LinkedList<Double> ();
              }
              
              if( Entity.Chunk.Chunk_Name.length() >= 7 && Entity.Chunk.Chunk_Name.substring(0,7).equals( "world3d")){
                //currently don't define "world3d-" visual buffer chunk.
              }
              else{
            	  // critical element case is in this 
                sim.funs.ChunkFun__Define_Chunk( Entity.Chunk );
              }
              
              sim.vars.visualBuffer.Visual_Buffer_Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              //sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Type = sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name.Substring(0, sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name.Length-(Visicon_History_Count-1).ToString().Length ); //e.g., Chunk_Type = "text"
              
              //also update Vision module for re-encoding, 
              //in addition, output eye movement results.
              String screen_pos = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "screen-pos");
              String eye_movement_result = Double.toString(GlobalUtilities.round (SimSystem.clock(),3)) + "\t" + screen_pos + "\t";
              if(   (screen_pos.length() >= 10 && screen_pos.substring(0,10).equals( "near-point")) ) {
                //sim.vars.visionModule.Last_Attended_Screen_X = "";
                //sim.vars.visionModule.Last_Attended_Screen_Y = "";
                //sim.vars.visionModule.Last_Attended_Visual_Location_Name = screen_pos;
                //sim.vars.visionModule.Last_Attended_Visicon_Name = old_chunk_name;
                //sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
              }
              else if ( (screen_pos.length() >= 9 && screen_pos.substring(0,9).equals( "far-point")) ){ 	
                //sim.vars.visionModule.Last_Attended_Screen_X = "";
                //sim.vars.visionModule.Last_Attended_Screen_Y = "";
                //sim.vars.visionModule.Last_Attended_Visual_Location_Name = screen_pos;
                //sim.vars.visionModule.Last_Attended_Visicon_Name = old_chunk_name;
                //sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
              }
              else if ( (screen_pos.length() >= 16 && screen_pos.substring(0,16).equals( "critical-element")) ){ 	 // this case is added by Yelly                  
                  // these commented codes are uncommented by Yelly
                  sim.vars.visionModule.Last_Attended_Screen_X = "";
                  sim.vars.visionModule.Last_Attended_Screen_Y = "";
                  sim.vars.visionModule.Last_Attended_Visual_Location_Name = screen_pos;
                  sim.vars.visionModule.Last_Attended_Visicon_Name = old_chunk_name;
                  sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
                }
              else if (screen_pos.equals( "customized-point-dummy-visual-location-point-vehicle-a")){
                  //do nothing
                }
              else{
                Chunk temp_visual_location_pointer = (Chunk)sim.vars.centralParametersModule.Chunks.get(screen_pos);
                sim.vars.visionModule.Last_Attended_Screen_X = sim.funs.ChunkFun__Get_Chunk_Slot_Value(temp_visual_location_pointer, "screen-x");
                sim.vars.visionModule.Last_Attended_Screen_Y = sim.funs.ChunkFun__Get_Chunk_Slot_Value(temp_visual_location_pointer, "screen-y");
                sim.vars.visionModule.Last_Attended_Visual_Location_Name = screen_pos;
                sim.vars.visionModule.Last_Attended_Visicon_Name = old_chunk_name;
                sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
                
                int the_visicon_id = sim.funs.VisionModuleFun__Find_The_Visicon_ID_By_Visicon_Name( sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name );
                Chunk the_visicon_pointer = sim.funs.ProgramUtilitiesFun__LinkedList_Get_i_th_Chunk_Pointer( sim.vars.visualDisplay.Visicon,  the_visicon_id );
                String the_text = sim.funs.ChunkFun__Get_Chunk_Slot_Value(the_visicon_pointer, "value");
                //GlobalUtilities.popUpMessage(the_text);
                eye_movement_result += sim.vars.visionModule.Last_Attended_Screen_X + "\t" + sim.vars.visionModule.Last_Attended_Screen_Y + "\t" + sim.vars.visionModule.Last_Attended_Visicon_Name + "\t" + the_text + "\t" +  the_text.length();
              }
              
              sim.funs.ProgramUtilitiesFun__Output_Eye_Movement_Results_Txt(eye_movement_result);
              
              if (!sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name.equals( "" ) && !sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Type.equals( "" )){
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "SET-BUFFER-CHUNK VISUAL " + sim.vars.visualBuffer.Visual_Buffer_Chunk.Chunk_Name); 
              }
              else 	sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "VISION" + "\t\t" + "No visual-object found"); 
              
            }
            else if (Entity.Entity_Type.equals( "Visual Clear Notice")) { //this is implicit visual buffer strict harvesting or explicit -visual>
              
              //reset last attended info should only be done by +visual> isa clear.
              //sim.vars.visionModule.Last_Attended_Screen_X ="";
              //sim.vars.visionModule.Last_Attended_Screen_Y ="";
              //sim.vars.visionModule.Last_Attended_Visual_Location_Name = "";
              //sim.vars.visualization__Vision_Module_Last_Attended_Visicon_Name = sim.vars.visionModule.Last_Attended_Visicon_Name;
              //sim.vars.visionModule.Last_Attended_Visicon_Name = "";
              
            } //currently don't know what should do
            else if (Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice") ) {
              // nothing need to do now
              if (sim.vars.messageOn) System.out.println ("Chunk Spec Changed at Visual_Buffer" );
            }
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Visicon();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Buffer_Contents();
            sim.funs.NetworkDetailsVisualizationFun__Get_Visual_Location_Buffer_Contents();
            
          case Ending:
            
            
            if (Entity.Entity_Type.equals( "Visual Buffer's New Chunk") || Entity.Entity_Type.equals( "Production Rule Firing Trigger") || Entity.Entity_Type.equals( "Buffer Chunk Spec Change Notice")) {
              Entity.From = "Visual Buffer"; //change the entity and pass it to Matching And Selection as a trigger
              Entity.To = "Matching And Selection";
              Entity.Entity_Type = "Production Rule Firing Trigger";
            //no need. if change Tag, must update server.qnactrEntityInServer and server.removeTokensInServer
              //Entity.Tag = QnactrSimulation.entityNumber; 
              //QnactrSimulation.entityNumber++;

            }
            
            //ProgramUtilitiesFun__Obsolete_Clean_All_Trash();
            
            //if(Entity.ID.equals( "Visual Display Change Notice"))Entity.ID = ""; //undo merge settings.
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            	
        }
        break;
        
        
      case visualdisplay:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true; //timing in timing section
            //can process multiple entities at the same time.
            
            
            
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //GlobalUtilities.popUpMessage(Entity.Entity_Type);
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            if (Entity.Trash ) { //pass
              //if multiple visual display offset entities for the same visicon are in this node, all the entities other than the first one will go here and are then ended.
            }
            else if( Entity.Entity_Type.equals( "Visual Display Onset") ){ 
              //ProgramUtilitiesFun__Output_Trace_Txt("Visual Display Onset");
              Chunk temp_chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              //check if_onset_clear before adding the visicon
              if (temp_chunk.Slot.containsKey(":onset-clear") && temp_chunk.Slot.get(":onset-clear").equals( "t" ) ) sim.funs.DeviceModuleFun__Clear_Visual_Display();
              
              temp_chunk.Creation_Time = SimSystem.clock();
              sim.vars.visualDisplay.Visicon.addLast(temp_chunk);
              sim.vars.visualization__Visual_Display = sim.funs.DeviceModuleFun__Get_Visual_Display (); //this is the old visualization method
              sim.funs.AnimatorModuleFun__Add_Visicon( temp_chunk ); //this is the new visualization method
            }
            else if (Entity.Entity_Type.equals( "Visual Display Offset")) {
              //GlobalUtilities.popUpMessage( sim.vars.visualDisplay.Visicon.Count );
              
              int temp_id = sim.funs.VisionModuleFun__Find_The_Visicon_ID_By_Visicon_Name(Entity.Chunk.Chunk_Name);
              //GlobalUtilities.popUpMessage("Visual display offset visicon Entity.Chunk.Chunk_Name: " + Entity.Chunk.Chunk_Name + ", temp_id: " + temp_id);	
              if (temp_id < 0) { //the visicon has been cleared by a clear-screen command
                //GlobalUtilities.popUpMessage("Error! Visual Display ending effect has temp_id < 0" + ". Chunk name: " + Entity.Chunk.Chunk_Name);
                Entity.Trash = true; //ignore the entity //all visual display offset entities in this task node should have been trashed by sim.funs.DeviceModuleFun__Clear_Visual_Display();
              }	
              else{
                
                //first need to trash any pending offset entities for the same visicon in this node.
                String visicon_name = Entity.Chunk.Chunk_Name;
                List<Entity> all_offset_entities_for_this_visicon = new ArrayList<Entity>();
                
                Iterator<Entity> itr_entities = sim.funs.getLinkedListOfQnactrEntityInServerAndQueue("visualdisplay").iterator(); 
                while(itr_entities.hasNext()){
                  Entity temp_entity = itr_entities.next();

                  if( temp_entity.Entity_Type.equals( "Visual Display Offset") && temp_entity.Chunk.Chunk_Name.equals( visicon_name )){
                    all_offset_entities_for_this_visicon.add(temp_entity );
                  }
                }
                //GlobalUtilities.popUpMessage("Visual display ending effect all_offset_entities_for_this_visicon.Count:" +  all_offset_entities_for_this_visicon.Count);
                if( all_offset_entities_for_this_visicon.size() > 1){ //when it =1, it means no other offset entity rather than itself
                  for(Entity temp_entity : all_offset_entities_for_this_visicon){
                    temp_entity.Trash = true;
                  }
                }
                //this entity itself may also be trashed. change it back.
                Entity.Trash = false;
                
                
                //
                Chunk temp_chunk = sim.funs.ProgramUtilitiesFun__LinkedList_Get_i_th_Chunk_Pointer( sim.vars.visualDisplay.Visicon,  temp_id);
                sim.vars.visionModule.Visual_Finst_ID_Track.remove((Object)temp_id); // remove the Visual Finst ID tracker, temp_id is not LinkedList index
                sim.funs.AnimatorModuleFun__Remove_Visicon( temp_chunk ); //this is the new visualization method
                sim.vars.visualDisplay.Visicon.remove( temp_chunk ) ; //remove the visicon
                
                
                
                //check offset_clear
                if (temp_chunk.Slot.containsKey(":offset-clear") && temp_chunk.Slot.get(":offset-clear").equals( "t") ) {
                  boolean have_something_before_clear;
                  if ( sim.vars.visualDisplay.Visicon.size() > 0 ) have_something_before_clear = true;
                  else have_something_before_clear = false;
                  sim.funs.DeviceModuleFun__Clear_Visual_Display();
                  if ( have_something_before_clear){  //print this only once
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "NONE" + "\t\t" + "CLEAR-SCREEN");
                  }
                }
                //GlobalUtilities.popUpMessage( sim.vars.visualDisplay.Visicon.Count );
                sim.vars.visualization__Visual_Display = sim.funs.DeviceModuleFun__Get_Visual_Display (); //this is the old visualization method
              }
            }
            else if (Entity.Entity_Type.equals( "Clear Visual Display")){
              boolean have_something_before_clear;
              if ( sim.vars.visualDisplay.Visicon.size() > 0 ) have_something_before_clear = true;
              else have_something_before_clear = false;
              sim.funs.DeviceModuleFun__Clear_Visual_Display();
              if ( have_something_before_clear){  //print this only once
                sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "NONE" + "\t\t" + "CLEAR-SCREEN");
              }
            }
            else if (Entity.Entity_Type.equals( "Visual World3D Onset")){
              sim.vars.visualDisplay.World3D_Visible_Object_IDs.addLast(Entity.From);	
            }
            else if (Entity.Entity_Type.equals( "Visual World3D Offset")){
              if(!sim.vars.visualDisplay.World3D_Visible_Object_IDs.contains(Entity.From)){
                System.out.println("Error! Visual Display Visual World3D Offset does not contain Entity.From: " + Entity.From );
                break;
              }
              sim.vars.visualDisplay.World3D_Visible_Object_IDs.remove(Entity.From);	
            }
            else System.out.println("Visual Display has undefined entity_type: " + Entity.Entity_Type);
            
            
            Entity.Scheduled_Task_Enter_Clock_Time = (double) 0.0; //change back to default for safe
            Entity.From = "Visual Display"; //tag from and to
            Entity.To = "Vision Module";
            Entity.Entity_Type = "Visual Display Change Notice";  
            
            
            sim.funs.NetworkDetailsVisualizationFun__Get_Visicon();
            
            
            
            
            
            break;
            
          case Timing:
            if( GlobalUtilities.round (SimSystem.clock(),3) > GlobalUtilities.round(Entity.Scheduled_Task_Enter_Clock_Time,3 )) { //otherwise, if the Scheduled time was just assigned with the Clock time, the new Clock time, though is still the same, will be greater than the scheduled time
              Entity.Time_Computed = true;
              return 0.0;
            }
            else {
              Entity.Time_Computed = true;
              return GlobalUtilities.round((Entity.Scheduled_Task_Enter_Clock_Time - SimSystem.clock()),3);
            }
            
            /*
        		double duration = 0.0;
        		if (Entity.Entity_Type.equals( "Visual Display Duration Control")) duration = Double.valueOf ( Entity.Chunk.Chunk_Name );
        		//GlobalUtilities.popUpMessage(timing);
        		return duration;
             */
            	
        }
        break;
        
        
      case vocalbuffer:
        switch (ServiceStage){
          case Release:
            
            
            int itemsInTask = sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString());
            //return (!Entity.Trash ) && (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //(itemsInTask < 1);
            return true;
            
          case Beginning:
            Entity.Time_Computed = false;
            sim.funs.NetworkDetailsVisualizationFun__Get_Speech_Details();
            
            
            break;
            
          case Ending:
            
            
            Entity.Chunk = null;
            Entity.Response_Item = null;
            Entity.From = "Vocal Buffer";
            Entity.To = "Matching And Selection";
            Entity.Entity_Type = "Production Rule Firing Trigger";
            
            
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0;
            	
        }
        break;
      
        
      case entitydirectcastdelay:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
          case Beginning:
            // no beginning
            break;
            
          case Ending:
            
            //after delay
            //send to destination
            Entity Temp_Entity = sim.funs.createEntity( Entity.To , "", Entity.To, "", 0.0);
            
            Entity.copyPropertiesOtherThanTagTo(Temp_Entity);
            
            //          Entity.ID = (String)sim.vars.programGlobalVar__Hashtable_Server_Node_Name_To_ID.get(Entity.To);
            Entity.Direct_Cast_Delay = (double) 0.0;
            //            Entity.Time = (double) SimSystem.clock();
            //            Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
            //            QnactrSimulation.entityNumber++;
                
            break;
            
          case Timing:
            return Entity.Direct_Cast_Delay;
            
              
        }
        break;
        
        
        
      case recurrentevent:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
          case Beginning:
            
            if( Entity.Entity_Type.equals("Recurrent Event 1") &&  SimSystem.clock() == 0.0){
              // initialize              

              //perceptual sub-network
              //vision module
              sim.vars.utilization__Vision_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "vision module" , sim.vars.utilization__Vision_Module_Changes_In_A_Second );
  
              //audio module
              sim.vars.utilization__Audio_Module_In_A_Second  = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "audio module" ,  sim.vars.utilization__Audio_Module_Changes_In_A_Second  );
  
              //cognitive sub-network
              //production module
              sim.vars.utilization__Production_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "production module" , sim.vars.utilization__Production_Module_Changes_In_A_Second );
  
              //declarative module
              sim.vars.utilization__Declarative_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "declarative module" , sim.vars.utilization__Declarative_Module_Changes_In_A_Second );
  
              //imaginary module
              sim.vars.utilization__Imaginary_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "imaginary module",  sim.vars.utilization__Imaginary_Module_Changes_In_A_Second );
  
              //intentional module not included, because it never takes time.
  
  
              //motor sub-network
              //motor module
              sim.vars.utilization__Motor_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "motor module" , sim.vars.utilization__Motor_Module_Changes_In_A_Second );
  
              //speech module
              sim.vars.utilization__Speech_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "speech module", sim.vars.utilization__Speech_Module_Changes_In_A_Second );
            }
            
            
            break;
            

          case Timing:
            return Entity.Direct_Cast_Delay;
            
            
            
          case Ending:

            //may add logics here to terminate a recurrent event by setting .Trash = true
            
            if( Entity.Entity_Type.equals("Recurrent Event 1") ){
              
              //sim.vars.utilization__Clock_Minus_1 = GlobalUtilities.round( SimSystem.clock(), 3 ) - 1 ;
                            
              double clock_minus_half = GlobalUtilities.round( SimSystem.clock(), 3 ) - 0.5 ; 
              //perhaps Utilization__Clock_Minus_Half  - 0.5 is better than Minus_1
              //i.e., t = 0.5   the mental workload single value is the average from t = 0 to t = 1.

              
              //perceptual sub-network
              //vision module
              sim.vars.utilization__Vision_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "vision module" , sim.vars.utilization__Vision_Module_Changes_In_A_Second );
  
              //audio module
              sim.vars.utilization__Audio_Module_In_A_Second  = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "audio module" ,  sim.vars.utilization__Audio_Module_Changes_In_A_Second  );
  
              sim.vars.utilization__Perceptual_SubNetwork_In_A_Second = ( sim.vars.utilization__Vision_Module_In_A_Second + sim.vars.utilization__Audio_Module_In_A_Second )  / 2.0;
  
  
              //cognitive sub-network
              //production module
              sim.vars.utilization__Production_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "production module" , sim.vars.utilization__Production_Module_Changes_In_A_Second );
  
              //declarative module
              sim.vars.utilization__Declarative_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "declarative module" , sim.vars.utilization__Declarative_Module_Changes_In_A_Second );
  
              //imaginary module
              sim.vars.utilization__Imaginary_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "imaginary module",  sim.vars.utilization__Imaginary_Module_Changes_In_A_Second );
  
              //intentional module not included, because it never takes time.
  
              sim.vars.utilization__Cognitive_SubNetwork_In_A_Second = (sim.vars.utilization__Production_Module_In_A_Second + sim.vars.utilization__Declarative_Module_In_A_Second + sim.vars.utilization__Imaginary_Module_In_A_Second ) / 3.0;
  
  
              //motor sub-network
              //motor module
              sim.vars.utilization__Motor_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "motor module" , sim.vars.utilization__Motor_Module_Changes_In_A_Second );
  
              //speech module
              sim.vars.utilization__Speech_Module_In_A_Second = sim.funs.UtilizationFun__Get_Utilization_Of_A_Module_From_Changes_In_A_Second ( "speech module", sim.vars.utilization__Speech_Module_Changes_In_A_Second );
  
              sim.vars.utilization__Motor_SubNetwork_In_A_Second = (sim.vars.utilization__Motor_Module_In_A_Second  + sim.vars.utilization__Speech_Module_In_A_Second ) / 2.0; 
            
              double overall_Utilization_In_A_Second =  (sim.vars.utilization__Perceptual_SubNetwork_In_A_Second + sim.vars.utilization__Cognitive_SubNetwork_In_A_Second + sim.vars.utilization__Motor_SubNetwork_In_A_Second) / 3.0;
              
              //output results in .txt
              sim.files.Results_mental_workload.writeLine(clock_minus_half + "\t" + sim.vars.utilization__Vision_Module_In_A_Second + "\t" +sim.vars.utilization__Audio_Module_In_A_Second + "\t" + sim.vars.utilization__Perceptual_SubNetwork_In_A_Second + "\t" + sim.vars.utilization__Production_Module_In_A_Second + "\t" + sim.vars.utilization__Declarative_Module_In_A_Second + "\t" + sim.vars.utilization__Imaginary_Module_In_A_Second + "\t" + sim.vars.utilization__Cognitive_SubNetwork_In_A_Second + "\t" + sim.vars.utilization__Motor_Module_In_A_Second + "\t" + sim.vars.utilization__Speech_Module_In_A_Second + "\t" + sim.vars.utilization__Motor_SubNetwork_In_A_Second + "\t" + overall_Utilization_In_A_Second);
              
            }
            
            //if all non-ended entities are recurrent events, stop all recurrent events one by one
            boolean allNonEndedEntitiesAreRecurrentEvents = true;  
            Iterator<Entity> itr = QnactrSimulation.getNotEndedGlobalAllEntitiesList().iterator();
            while(itr.hasNext()){
              Entity anEntity = itr.next();
              if(  !anEntity.From.equals("Recurrent Event")  ||  !anEntity.To.equals("Recurrent Event") ) {
                //this entity is not a recurrent event
                allNonEndedEntitiesAreRecurrentEvents = false;
                break;
              }
            }
            if(allNonEndedEntitiesAreRecurrentEvents) Entity.Trash = true;
            
            break;
            
              
        }
        break;
        
        
      case timertriggeringfeedbackordisplay:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;  
            
            
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //System.out.println("flag in Timer Triggering Feedback Or Display");  
            break;
            
          case Ending:
            Entity.Scheduled_Task_Enter_Clock_Time = (double) 0.0;
            Entity.From = "Timer Triggering Feedback Or Display";
            Entity.To =   "Visual and Audio Display Schedule";
            
            
            break;
            
          case Timing:
            if( GlobalUtilities.round(SimSystem.clock(),3 ) > GlobalUtilities.round(Entity.Scheduled_Task_Enter_Clock_Time,3)) { //otherwise, if the Scheduled time was just assigned with the Clock time, the new Clock time, though is still the same, will be greater than the scheduled time
              Entity.Time_Computed = true;
              return 0.0;
            }
            else{
              Entity.Time_Computed = true;
              return GlobalUtilities.round((Entity.Scheduled_Task_Enter_Clock_Time - SimSystem.clock()),3);
            }
            
            //return GlobalUtilities.round((Entity.Scheduled_Task_Enter_Clock_Time - Clock), 3);
            
            
              
        }
        break;
        
        
        
      case visualandaudiodisplayschedule:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            //all visual and audio display changes at each time instance should be handled in this task node
            
            
            
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            /*
          if(Entity.Entity_Type.equals( "Display Initialization")) {
            Entity.Chunk.Chunk_Type = "Waiting Time";
            Entity.Chunk.Chunk_Name = "0.0";
          }
             */
            
//            System.out.println("ServerLogics, " + ServerName + ", " + ServiceStage + ". " + Entity.Entity_Type + " Done.");
            
            
            break;
            
          case Ending:
            if(serverLogicsDebugPopupFlag)JOptionPane.showMessageDialog(null, "Clock: " + SimSystem.clock() + ". Operator ID: " + sim.ID  + "\nEnums has serverName: " + ServerName + ". ServiceStage: " + ServiceStage, "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
            if(Entity.Entity_Type.equals( "Display Initialization")){ // only once
              if(!sim.vars.taskTemplate.Method.equals( ""))  sim.funs.TaskTemplateFun__Start_Trial_Display(); //this function will change task stage to Display_And_Response
            } 
            else if (Entity.From.equals( "Control To Display") && Entity.To.equals( "Visual and Audio Display Schedule")){ //respond to control events
              if ( Entity.Entity_Type.equals( "Self Speech")  ){ //for self-hearing
                if ( sim.vars.audioModule.Listening_To_OnesOwn_Speech == true){
                  String word = (String) Entity.Chunk.Slot.get("string");
                  String location = "self"; //this is one's own voice heared by one's own ear, which is not subvocalize (not really make any sound) Speech Execution
                  //System.out.println("Visual and Audio Display Schedule get Self Speech");
                  sim.funs.DeviceModuleFun__Audio_Display_Prepare_Word_Sound(word, "", location); //word, onset_set(optional, Clock by default), location (optional, "external" by default)
                }
              }
              else if(sim.vars.taskTemplate.Method.equals( "single_discrete_task_visual_display") || sim.vars.taskTemplate.Method.equals( "discrete_display_feedback_two_stages_method")) { //know to use only Task_1, because they are single task templates
                if (sim.vars.taskTemplate.Task_1_Current_Stage.equals( "Display_And_Response" )) {
                  

                  Iterator<Entity> itr_entities = sim.funs.getLinkedListOfQnactrEntityInServerAndQueue("timertriggeringfeedbackordisplay").iterator(); //  Timer Triggering Feedback Or Display, ID 203
                  while(itr_entities.hasNext()){
                    Entity temp_entity = itr_entities.next();

                    System.out.println( "Visual and Audio Display Schedule ToDo disable any  Feedback Stage Trigger  in        Timer Triggering Feedback or Display");
                    //ToDo: disable any "Feedback Stage Trigger" in        Timer Triggering Feedback or Display
                  }
                  
                  sim.funs.TaskTemplateFun__Start_Trial_Feedback(); //this function will change task stage to Feedback
                }
                else if (sim.vars.taskTemplate.Task_1_Current_Stage.equals( "Feedback" )) {
                  
                  
                  
                  sim.funs.TaskTemplateFun__Start_Trial_Display(); //this function will change task stage to Display_And_Response
                }
                else System.out.println ("Visual and Audio Display Schedule has sim.vars.taskTemplate.Task_1_Current_Stage an undefined case in ending effect");
              }
              else System.out.println("Visual and Audio Display Schedule has sim.vars.taskTemplate.Method == " + sim.vars.taskTemplate.Method + " undefined");
              
            } //respond to control events
            else if (Entity.From.equals( "Timer Triggering Feedback Or Display") && Entity.To.equals( "Visual and Audio Display Schedule")) {// respond to timer events //seems very similar with respond to control event
              
              if(sim.vars.taskTemplate.Method.equals( "single_discrete_task_visual_display") || sim.vars.taskTemplate.Method.equals( "discrete_display_feedback_two_stages_method")) {
                if (sim.vars.taskTemplate.Task_1_Current_Stage.equals( "Display_And_Response")  && Entity.Entity_Type.equals(  "Feedback Stage Trigger")) {
                  sim.funs.TaskTemplateFun__Start_Trial_Feedback(); //this function will change task stage to Feedback
                }
                else if (sim.vars.taskTemplate.Task_1_Current_Stage.equals( "Feedback" ) &&  Entity.Entity_Type.equals( "Display Stage Trigger")) {
                  //System.out.println("flag at Visual and Audio Display Schedule");
                  sim.funs.TaskTemplateFun__Start_Trial_Display(); //this function will change task stage to Display_And_Response
                }
                else System.out.println ("Visual and Audio Display Schedule has sim.vars.taskTemplate.Task_1_Current_Stage an undefined case in ending effect");
              }
              else System.out.println("Visual and Audio Display Schedule has sim.vars.taskTemplate.Method == " + sim.vars.taskTemplate.Method + " undefined");
              
            } // respond to timer events
            else System.out.println("Error! Visual and Audio Display Schedule has an undefined case" );
            
            break;
            
          case Timing:
            /*if (Entity.Chunk.Chunk_Type.equals( "Waiting Time")){ //waiting time specified
          Entity.Time_Computed = true;
          return Double.valueOf( Entity.Chunk.Chunk_Name );
        }
        else {
             */
            
            Entity.Time_Computed = true;
            return 0.0;
            
            //}
            
              
        }
        break;
        
      case controltodisplay:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
          case Beginning:
            Entity.Time_Computed = true;
            break;
            
          case Ending:
            
                        
            //extra entity to trigger self speech perception
            if(Entity.From.equals( "Control Voice Key") && Entity.Entity_Type.equals( "Self Speech")){//this is common for all models
              Entity Temp_Entity = sim.funs.createEntity( "Visual and Audio Display Schedule" , "Control To Display", "Visual and Audio Display Schedule", "Self Speech", 0.0);
              
              //              Entity Temp_Entity = new Entity();  
              //              Temp_Entity.ID = "201"; // Visual and Audio Display Schedule
              //              Temp_Entity.Time = (double) SimSystem.clock();
              //              Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
              //              QnactrSimulation.entityNumber++;
              //              
              //              Temp_Entity.From = "Control To Display";
              //              Temp_Entity.To = "Visual and Audio Display Schedule";
              //              Temp_Entity.Entity_Type = "Self Speech" ;
              Temp_Entity.Chunk = sim.funs.ChunkFun__Chunk_Clone(Entity.Chunk);
              
              
              Entity.Entity_Type = "" ; //change the current entity to a normal entity, triggering the response recording process as normal.
            }
            
            if(sim.vars.taskTemplate.Method.equals( "discrete_display_feedback_two_stages_method") ){ //know to use only Task_1
              String current_stage = sim.vars.taskTemplate.Task_1_Current_Stage;
              if ( current_stage.equals( "Display_And_Response") &&  !(Entity.Response_Item instanceof Response_Item_Move_Cursor)){ //still collecting response,  //currently do not record move cursor response
                boolean terminate_display_stage = false;
                
                Hashtable current_result_table_pointer = sim.vars.centralParametersModule.Experiment_Trial_Result_List.getLast();
                LinkedList<Object> current_response_list_pointer = (LinkedList<Object>) sim.vars.centralParametersModule.Experiment_Trial_Response_Item_List.getLast();
                
                int number_of_responses_needed =  (int) current_result_table_pointer.get("number-of-responses-needed");
                int responsed_collected = current_response_list_pointer.size();
                
                //add the response item carried by the incoming Entity to result and response list // by default, just record reaction content and response clock time. if auto compute correctness and reaction time are set to true, the ith reponse will use display item i 's appear time as start time, and it correct response as the correct response.
                if (Entity.Response_Item != null ){
                  double display_stage_start_clock = (double) current_result_table_pointer.get("display-stage-start-time");
                  //here assume display item i is matched with response item i.
                  int item_num = responsed_collected; //start from 0
                  //                double item_delay = Double.valueOf( (String)  current_result_table_pointer[ "item-" + item_num.ToString() + ":display_item_delay"] ); //e.g., item-0:display_item_delay
                  //stopped using item_delay in auto reaction time computation, because in multi-task, the first response may not correspond to the first display. instead, in case of delay used or multi-task, post processing in Excel to figure out the correct reaction time.
                  
                  int global_trial_num = (int)current_result_table_pointer.get("global-trial-num");
                  if( global_trial_num != sim.vars.centralParametersModule.Experiment_Trial_Result_List.size()) System.out.println("Error. Control To Display  global_trial_num != Central_Parameters_Module.Experiment_Trial_Result_List.Count");
                  
                  LinkedList<Object> all_display_items_in_this_trial = (LinkedList<Object>) sim.funs.ProgramUtilitiesFun__LinkedListObject_Get_i_th_Object( sim.vars.centralParametersModule.Experiment_Trial_Display_Item_List, global_trial_num) ;
                  
                  
                  
                  //can take more responses than display items, but for these extra responses, correctness cannot be automatically computed.        
                  String correct_response;
                  if(  all_display_items_in_this_trial.size() >=  item_num + 1  ) { //currently cannot record more response per trial than the display items number in the trial
                    Object a_display_item = sim.funs.ProgramUtilitiesFun__LinkedListObject_Get_i_th_Object( all_display_items_in_this_trial ,  item_num + 1 ) ;
                    correct_response = sim.funs.TaskTemplateFun__Get_Correct_Response_From_Display_Item(a_display_item );
                  }
                  else correct_response = ""; 
                  
                  double reaction_clock = -1.0;
                  double reaction_time = -1.0;
                  String response =null;
                  boolean response_correctness = false;
                  Object a_clone_response_item = new Object ();
                  
                  if(Entity.Response_Item instanceof  Response_Item_Key_Press){ //include both keyboard and mouse key press, and touch screen tap
                    reaction_clock = ((Response_Item_Key_Press) Entity.Response_Item).Clock_Time;
                    reaction_time =  reaction_clock  -  display_stage_start_clock; // reaction_time =  reaction_clock  -  display_stage_start_clock  - item_delay;
                    ((Response_Item_Key_Press) Entity.Response_Item).Reaction_Time = (double) reaction_time;
                    response = ((Response_Item_Key_Press) Entity.Response_Item).Key;
                    
                    //add mouse cursor location information to mouse click responses.
                    if (response.equals( "mouse-L") || response.equals( "mouse-R" )) {
                      String mouse_cursor_x = Integer.toString(sim.vars.deviceModule.Mouse_Cursor_Screen_X);
                      String mouse_cursor_y = Integer.toString(sim.vars.deviceModule.Mouse_Cursor_Screen_Y);
                      response = response + "(" + mouse_cursor_x + "," + mouse_cursor_y + ")" ;
                      // ((Response_Item_Key_Press) Entity.Response_Item).Key = response; //currently do not add this line, because further codes in this function needs to match "moust-L" key
                      ((Response_Item_Key_Press) Entity.Response_Item).Mouse_Cursor_X = Integer.parseInt(mouse_cursor_x);
                      ((Response_Item_Key_Press) Entity.Response_Item).Mouse_Cursor_Y = Integer.parseInt(mouse_cursor_y);
                    }
                    
                    
                    if (sim.funs.ProgramUtilitiesFun__StringsEqualByStringOrDouble(response, correct_response)) response_correctness = true;
                    else response_correctness = false;
                    ((Response_Item_Key_Press) Entity.Response_Item).Response_Correctness = response_correctness;
                    a_clone_response_item = (Response_Item_Key_Press) sim.funs.ProgramUtilitiesFun__Response_Item_Clone( Entity.Response_Item ) ;
                    
                    //old mouse L click specific
                    //if (((Response_Item_Key_Press) Entity.Response_Item).Key.equals( "mouse-L")) {//capture any event that can be triggered by mouse left click
                    //  sim.funs.TaskTemplateFun__Catch_Mouse_L_Click_Event( sim.funs.ProgramUtilitiesFun__Response_Item_Clone( Entity.Response_Item ) );
                    //}
                    
                    //new any key press including keyboard and mouse
                    sim.funs.TaskTemplateFun__Catch_Key_Press_Event(Entity.Response_Item);
                  }
                  
                  else if(Entity.Response_Item instanceof  Response_Item_Move_Cursor){
                    /*
                    //currently do not record move cursor response, the large "if" is broke, so it is impossible to reach this.
                    reaction_clock = ((Response_Item_Move_Cursor) Entity.Response_Item).Clock_Time;
                    reaction_time =  reaction_clock  -  display_stage_start_clock  - item_delay;
                      ((Response_Item_Move_Cursor) Entity.Response_Item).Reaction_Time = reaction_time;
                    
                    
                    response = ((Response_Item_Move_Cursor) Entity.Response_Item).End_X;
                    if (response.equals( correct_response)) response_correctness = true;
                    else response_correctness = false;
                      ((Response_Item_Move_Cursor) Entity.Response_Item).Response_Correctness = response_correctness;
                      
                      
                    a_clone_response_item = (Response_Item_Move_Cursor) sim.funs.ProgramUtilitiesFun__Response_Item_Clone( Entity.Response_Item ) ;
                     */
                  }
                  else if(Entity.Response_Item instanceof  Response_Item_Move_Hand_Touch){
                	//currently do not record move hand response
                	  
                  }
                  else if(Entity.Response_Item instanceof Response_Item_Speech){
                    
                    reaction_clock = ((Response_Item_Speech) Entity.Response_Item).Clock_Time;
                    reaction_time =  reaction_clock  -  display_stage_start_clock ; // reaction_time =  reaction_clock  -  display_stage_start_clock  - item_delay;
                    ((Response_Item_Speech) Entity.Response_Item).Reaction_Time = (double) reaction_time;
                    response = ((Response_Item_Speech) Entity.Response_Item).Speech;
                    if (sim.funs.ProgramUtilitiesFun__StringsEqualByStringOrDouble(response, correct_response)) response_correctness = true;
                    else response_correctness = false;
                    ((Response_Item_Speech) Entity.Response_Item).Response_Correctness = response_correctness;
                    a_clone_response_item = (Response_Item_Speech) sim.funs.ProgramUtilitiesFun__Response_Item_Clone( Entity.Response_Item ) ;
                  }
                  else System.out.println("Control To Display has undefined Entity.Response_Item type");
                  
                  //add result to result list
                  current_result_table_pointer.put ("response-" + item_num + ":reaction-clock" , reaction_clock);
                  
                  if (response == null) response = "";
                  current_result_table_pointer.put ("response-" + item_num + ":response" , response);
                  
                  if(sim.vars.taskTemplate.Auto_Compute_Default_Reaction_Time) current_result_table_pointer.put ("response-" + item_num + ":reaction-time" , GlobalUtilities.round( reaction_time , 3) );  //round to 1 milisecond
                  if(sim.vars.taskTemplate.Auto_Compute_Default_Response_Correctness) current_result_table_pointer.put ("response-" + item_num + ":response_correctness" , response_correctness);
                  
                  //add result to response_item_list
                  current_response_list_pointer.addLast ( a_clone_response_item ); //otherwise, when the entity is gone, the response item (pointer) will point to null
                  
                  
                } //end of if (Entity.Response_Item != null)
                
                
                
                
                
                //task specific response computatio and collection codes go here, for tasks that cannot fit into any currently availablt template solution
                //
                //
                //
                //if certain criteria is met, change something on the display. OR add something to the result table using Hashtable current_result_table_pointer (String String)
                
                
                
                //for shooting dual task
                /*
              String response_key_for_shooting = "";
              String response_key_for_adding  = "" ;
              if( current_result_table_pointer.ContainsKey( "response-0:response" ) && ( (String)current_result_table_pointer["response-0:response"]).Length >= 7 && ( (String)current_result_table_pointer["response-0:response"]).Substring (0, 7).equals( "mouse-L")){
                response_key_for_shooting = "response-0";
              }
              else if ( current_result_table_pointer.ContainsKey( "response-1:response" ) && ( (String)current_result_table_pointer["response-1:response"]).Length >= 7 && ( (String)current_result_table_pointer["response-1:response"]).Substring (0, 7).equals( "mouse-L")){
                response_key_for_shooting = "response-1"; 
              }
              
              if( current_result_table_pointer.ContainsKey( "response-0:response" ) && ( sim.funs.ProgramUtilitiesFun__Is_String_Int( (String)current_result_table_pointer["response-0:response"])) ){
                response_key_for_adding = "response-0";
              }
              else if( current_result_table_pointer.ContainsKey( "response-1:response"  ) && ( sim.funs.ProgramUtilitiesFun__Is_String_Int( (String)current_result_table_pointer["response-1:response"])) ){
                response_key_for_adding = "response-1";
              }
              
              if( !response_key_for_shooting.equals( "" ) ) {
                
                double target_x = Double.valueOf( (String)current_result_table_pointer[":block_variable_x"]) ;
                double target_y = Double.valueOf( (String)current_result_table_pointer[":block_variable_y"]) ;
                double target_height = Double.valueOf( (String)current_result_table_pointer[":block_variable_height"]) ;
                double target_width  = Double.valueOf( (String)current_result_table_pointer[":block_variable_width"]) ;
                double hit_x = 0.0;
                double hit_y = 0.0;
                hit_x = (double) sim.vars.deviceModule.Mouse_Cursor_Screen_X;
                hit_y = (double) sim.vars.deviceModule.Mouse_Cursor_Screen_Y;

                //System.out.println(target_x + " " + target_y + ", " + hit_x + " " + hit_y);
                double radial_aiming_error_in_pixel = Math.Sqrt( (target_x - hit_x) *  (target_x - hit_x) + (target_y - hit_y) * (target_y - hit_y) ) ;
                double radial_aiming_error_in_inch = radial_aiming_error_in_pixel / target_height * 40;
                double radial_aiming_error_in_cm = radial_aiming_error_in_inch * 2.54;
                
                bool hit;
                double reactionTime = (double) current_result_table_pointer[response_key_for_shooting + ":reaction-clock"] - (double) current_result_table_pointer["display-stage-start-time"];
                reactionTime -= Double.valueOf( (String)current_result_table_pointer[":block_variable_visual_delay"]);
                
                //reactionTime < 3.0 for stress 5 cases
                if ( ( Math.Abs( hit_x - target_x ) < (target_width / 2.0) )  && ( Math.Abs(hit_y - target_y) < ( target_height/ 2.0) ) && reactionTime < 5.0) hit = true;
                else hit = false;
                
                bool is_friendly;
                if ( (String)current_result_table_pointer[":block_variable_target_type"].equals( "brown")) is_friendly = false;
                else if ( (String)current_result_table_pointer[":block_variable_target_type"].equals( "olive")) is_friendly = true;
                else { 
                  System.out.println("Control To Display shooting has undifined :block_variable_target_type: " + (String)current_result_table_pointer[":block_variable_target_type"]);
                  is_friendly = false;
                  SimSystem.abort();
                }
                
                bool friendly_hit, target_hit;
                if ( hit && !is_friendly ) target_hit = true;
                else target_hit = false;
                if ( hit && is_friendly ) friendly_hit = true;
                else  friendly_hit = false;
                
            //    System.out.println("target: " + target_x + " " + target_y + ", hit at: " + hit_x + " " + hit_y + ", error (CM): " +radial_aiming_error_in_cm + ", hit: " + hit);
                if( ! current_result_table_pointer.ContainsKey( "x_shooting_radial_aiming_error_in_cm") ) current_result_table_pointer.Add ( "x_shooting_radial_aiming_error_in_cm", radial_aiming_error_in_cm.ToString() );
            //    else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_shooting_radial_aiming_error_in_cm)");
                
                if( ! current_result_table_pointer.ContainsKey( "x_shooting_if_hit" ) ) current_result_table_pointer.Add ( "x_shooting_if_hit", hit.ToString() );
            //    else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_shooting_if_hit)");
                
                if( ! current_result_table_pointer.ContainsKey( "x_shooting_reactionTime" ) ) current_result_table_pointer.Add ( "x_shooting_reactionTime", reactionTime.ToString() );
            //    else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_shooting_reactionTime)");
                
                if( ! current_result_table_pointer.ContainsKey( "x_shooting_target_hit" ) ) current_result_table_pointer.Add ( "x_shooting_target_hit", target_hit.ToString() );
            //    else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_shooting_target_hit)");
                
                if( ! current_result_table_pointer.ContainsKey( "x_shooting_friendly_hit" ) ) current_result_table_pointer.Add ( "x_shooting_friendly_hit", friendly_hit.ToString() );
            //    else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_shooting_friendly_hit)");
              }
              
              if( !response_key_for_adding.equals( "" )) {
                double reactionTime = (double) current_result_table_pointer[response_key_for_adding + ":reaction-clock"] - (double) current_result_table_pointer["display-stage-start-time"];
                double visual_delay = Double.valueOf( (String)current_result_table_pointer[":block_variable_visual_delay"]);
                
                bool correctness;
                int adding_result = Convert.ToInt32( (String)current_result_table_pointer[response_key_for_adding + ":response"] );
                int number1 = Convert.ToInt32( (String)current_result_table_pointer["item-0:digit"] );
                int number2 = Convert.ToInt32( (String)current_result_table_pointer["item-1:digit"] );
                
                
                if ( (adding_result == number1 + number2)  )correctness = true;
                else correctness = false;
                if( ! current_result_table_pointer.ContainsKey( "x_adding_reactionTime" ) ) current_result_table_pointer.Add ( "x_adding_reactionTime", reactionTime.ToString() );
                //                else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_adding_reactionTime)");
                
                if( ! current_result_table_pointer.ContainsKey( "x_adding_correctness" ) ) current_result_table_pointer.Add ( "x_adding_correctness", correctness.ToString() );
                    //                else System.out.println("Control To Display has current_result_table_pointer.ContainsKey( x_adding_correctness)");
                
              }
                 */
                // end of shooting
                //
                
                
                
                // for ACT-R unit 3 sperling.lisp result gethering
                /*
              String frequency_string = (String) Central_Parameters_Module.Experiment_Trial_Result_List.Last.getValue()["item-12:frequency"]  ;
              String[] correct_response_list;
              if ( frequency_string.equals( "2000") ){ //high pitch and row
                correct_response_list = new String[] {"B", "C",  "D", "F"  };
              }
              else if ( frequency_string.equals( "1000") ){ //medium pitch and row
                correct_response_list = new String[] {"G", "H", "J", "K"    };
              }
              else { //500  //low
                correct_response_list = new String[] { "L",  "N", "M", "P"  };
              }
              
              LinkedList<String> correct_linkedList = sim.funs.ProgramUtilitiesFun__StringArray_To_LinkedList_String (correct_response_list );
              
              if(Entity.Response_Item is  Response_Item_Key_Press){
                String response_key = ((Response_Item_Key_Press) Entity.Response_Item).Key;
                if( correct_linkedList.Contains( response_key.ToUpper() ) ) aaa_Unit3_Sperling_Block_All_Correct_Responses ++ ;
              
              }
                 */
                
                //for ACT-R unit 6 bst-learn.lisp
                /*
                  
              if(Entity.Response_Item is  Response_Item_Key_Press &&  ((Response_Item_Key_Press) Entity.Response_Item).Key.equals( "mouse-L") ){
                int target_length = Convert.ToInt32( (String)current_result_table_pointer[":block_variable_target_end_x"]) - 50 ;
                int matching_length = aaa_BST_Learn_Blue_Line_Length;
                //System.out.println("target_length: " + target_length);
                if(sim.vars.deviceModule.Mouse_Cursor_Screen_X >= 10 && sim.vars.deviceModule.Mouse_Cursor_Screen_X <= 30 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y >= 25 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y <= 45){
                  //System.out.println("Button A clicked");
                  int A_length = Convert.ToInt32( (String)current_result_table_pointer[":block_variable_a_end_x"]) - 50 ;
                  if(matching_length > target_length){
                    matching_length = matching_length - A_length;
                  }
                  else{
                    matching_length = matching_length + A_length;
                  }
                }
                else if(sim.vars.deviceModule.Mouse_Cursor_Screen_X >= 10 && sim.vars.deviceModule.Mouse_Cursor_Screen_X <= 30 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y >= 50 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y <= 70){
                  //System.out.println("Button B clicked");
                  int B_length = Convert.ToInt32( (String)current_result_table_pointer[":block_variable_b_end_x"]) - 50 ;
                  if(matching_length > target_length){
                    matching_length = matching_length - B_length;
                  }
                  else{
                    matching_length = matching_length + B_length;
                  }
                }
                else if(sim.vars.deviceModule.Mouse_Cursor_Screen_X >= 10 && sim.vars.deviceModule.Mouse_Cursor_Screen_X <= 30 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y >= 75 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y <= 95){
                  //System.out.println("Button C clicked");
                  int C_length = Convert.ToInt32( (String)current_result_table_pointer[":block_variable_c_end_x"]) - 50 ;
                  if(matching_length > target_length){
                    matching_length = matching_length - C_length;
                  }
                  else{
                    matching_length = matching_length + C_length;
                  }
                }
                else if(sim.vars.deviceModule.Mouse_Cursor_Screen_X >= 10 && sim.vars.deviceModule.Mouse_Cursor_Screen_X <= 48 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y >= 125 && sim.vars.deviceModule.Mouse_Cursor_Screen_Y <= 145){
                  //System.out.println("Button Reset clicked");
                  matching_length = 0;
                }
                
                //update the current line
                //remove the old line
                if(!aaa_BST_Learn_Blue_Line_Visicon_Name.equals( "" ) ){
                  sim.funs.DeviceModuleFun__Visual_Display_Remove_Item_By_Visicon_Name ( aaa_BST_Learn_Blue_Line_Visicon_Name );
                  aaa_BST_Learn_Blue_Line_Visicon_Name = "";
                }
                //show the new line
                if(matching_length > 0 ) {
                  String visicon_name = sim.funs.DeviceModuleFun__Visual_Display_Prepare_Line(false, false, 50, 130, (50 + matching_length) , 135, Clock.ToString(), "", "blue"); //duration = "" means infinite display
                  aaa_BST_Learn_Blue_Line_Visicon_Name = visicon_name;
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("show blue line, length: " + matching_length );
                }
                else matching_length = 0;
                
                aaa_BST_Learn_Blue_Line_Length = matching_length;
              }
                 */
                
                
                
                
                
                //
                //
                //
                //
                
                
                
                
                
                //determine if to wait for more response items to come or trigger the next trial.
                if( sim.vars.taskTemplate.Response_Terminates_Display ){
                  if ( number_of_responses_needed == -1){
                    System.out.println("Error! Control To Display has sim.vars.taskTemplate.Response_Terminates_Display == true and number_of_responses_needed == -1");
                    terminate_display_stage = true;
                  }
                  else if ( number_of_responses_needed == 0 ){
                    System.out.println("Error! Control To Display has sim.vars.taskTemplate.Response_Terminates_Display == true and number_of_responses_needed == 0");
                    terminate_display_stage = true;
                  }
                  else if ( current_response_list_pointer.size() == number_of_responses_needed ){ // all responses collected
                    terminate_display_stage = true;
                  }
                }
                else { // not all the responses are collected, or the template only end the display stage when certain criteria is met
                  
                  //task specific, currently no template solution
                  //
                  //
                  //
                  //if certain criteria is met, set terminate_display_stage = true. then will change from display stage to feedback stage
                  
                  //for ACT-R unit 6 bst-learn.lisp
                  /*
                
                int target_length = Convert.ToInt32( (String)current_result_table_pointer[":block_variable_target_end_x"]) - 50 ;
                if( target_length == aaa_BST_Learn_Blue_Line_Length)  {
                  sim.funs.DeviceModuleFun__Visual_Display_Prepare_Text(false, false, "done" , 205, 206, Clock.ToString(), "", "", 50, 11);
                  
                  aaa_BST_Learn_Blue_Line_Length = 0;
                  aaa_BST_Learn_Blue_Line_Visicon_Name = "";
                  //terminate_display_stage = true;
                }
                   */
                  
                  //
                  //
                  //
                  //
                  
                  
                  //else do nothing, terminate_display_stage is still false     
                }
                
                //trash entity or not?
                if ( terminate_display_stage ){
                  //System.out.println("flag");
                  
                  /*//as in C# version
                Entity.Response_Item = null;
                Entity.From = "Control To Display";
                Entity.To = "Visual and Audio Display Schedule"; //moved into sim.funs.TaskTemplateFun__Terminate_Display_Response_Stage_Of_This_Trial
                
                
                foreach (Entity temp_entity in Model.Find ("ID", "203")){ //  Timer Triggering Feedback Or Display, ID 203
                  
                  System.out.println( "Control To Display ToDo disable any  Feedback Stage Trigger  in        Timer Triggering Feedback or Display");
                  //ToDo: disable any "Feedback Stage Trigger" in        Timer Triggering Feedback or Display
                } //moved to Visual and Audio Display Schedule
                
                   */
                  
                  sim.funs.TaskTemplateFun__Terminate_Display_Response_Stage_Of_This_Trial(0.0);
                  
                }
                else Entity.Trash = true; // do not terminate display stage, trash the entity
                
              }
              else if ( current_stage.equals( "Feedback" )){ 
                System.out.println("Control To Display ToDo: sim.vars.taskTemplate.Method == discrete_display_feedback_two_stages_method current_stage == Feedback  ");
                //Feedback responses are not collected, assume only 1 response per trial.
                //Todo, similar to the case above, need to disable any  Display Stage Trigger  in        Timer Triggering Feedback or Display
                // Currently, ignore any response in the feedback stage, so sim.vars.taskTemplate.Response_Terminates_Feedback won't work even when true.
              }
            }
            
            if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_medical_decision_making")){
              //System.out.println("Control To Display experiment_medical_decision_making.");
              String Key_Press_Event = "";
              double reaction_clock = -1.0;
              //double reaction_time = -1.0;
              String response =null;
              
              //only consider key / mouse click events, do not consider mouse move events
              if(Entity.Response_Item instanceof  Response_Item_Key_Press){ //include both keyboard and mouse key press
                reaction_clock = ((Response_Item_Key_Press) Entity.Response_Item).Clock_Time;
                //reaction_time =  reaction_clock  -  display_stage_start_clock; // reaction_time =  reaction_clock  -  display_stage_start_clock  - item_delay;
                //  ((Response_Item_Key_Press) Entity.Response_Item).Reaction_Time = reaction_time;
                response = ((Response_Item_Key_Press) Entity.Response_Item).Key;
                
                
                if(response.equals( "space") || response.equals( "d") || response.equals( "f")){ // here only for auditory space bar press event catch, and for memorization task, d f.
                  //add mouse cursor location information to mouse click responses.
                  if (response.equals( "mouse-L") || response.equals( "mouse-R") ) {
                    String mouse_cursor_x = Integer.toString(sim.vars.deviceModule.Mouse_Cursor_Screen_X);
                    String mouse_cursor_y = Integer.toString(sim.vars.deviceModule.Mouse_Cursor_Screen_Y);
                    response = response + "(" + mouse_cursor_x + "," + mouse_cursor_y + ")" ;
                    // ((Response_Item_Key_Press) Entity.Response_Item).Key = response; //currently do not add this line, because further codes in this function needs to match "moust-L" key
                    ((Response_Item_Key_Press) Entity.Response_Item).Mouse_Cursor_X = Integer.parseInt(mouse_cursor_x);
                    ((Response_Item_Key_Press) Entity.Response_Item).Mouse_Cursor_Y = Integer.parseInt(mouse_cursor_y);
                  }
                  
                  Key_Press_Event = response;
                  
                  Entity Temp_Entity = sim.funs.createEntity( "Dynamic Events" , "Control To Display", "Dynamic Events", Key_Press_Event, 0.0);
                  //                  Entity Temp_Entity = new Entity();  
                  //                  Temp_Entity.ID = "242"; // Dynamic Events
                  //                  Temp_Entity.Time = (double) SimSystem.clock();
                  //                  Temp_Entity.Tag = QnactrSimulation.entityNumber; //give it an entity number, init. 1
                  //                  QnactrSimulation.entityNumber++;
                  //                  
                  //                  Temp_Entity.From = "Control To Display";
                  //                  Temp_Entity.To = "Dynamic Events";
                  //                  Temp_Entity.Entity_Type = Key_Press_Event ; 
                  Temp_Entity.Response_Item = Entity.Response_Item;
       
                  
                  Entity.Trash = true;
                }
              }
            }
            
            
            
            /*    
          if (Entity.From.equals( "Control Motor" )|| Entity.From.equals( "Control Voice Key")){ //for any control event
            if(sim.vars.taskTemplate.Method.equals( "single_discrete_task_visual_display")) {
              Entity.From = "Control To Display";
              Entity.To = "Visual and Audio Display Schedule";
            }
            else if(sim.vars.taskTemplate.Method.equals( "discrete_display_feedback_two_stages_method" )){
              Entity.From = "Control To Display";
              Entity.To = "Visual and Audio Display Schedule";
            }
            else System.out.println("Control To Display has sim.vars.taskTemplate.Method == " + sim.vars.taskTemplate.Method + " undefined");
          }
             */
            
            
            /* //before merging visual and audio display schedule
          if (Entity.From.equals( "Control Motor")){ //for any kind of keyboard event
            Entity.From = "Control To Display";
            Entity.To = "Visual Display Schedule";
          }

          if(Entity.From.equals( "Control Voice Key") && Entity.Entity_Type.equals( "Self Speech")){//this is common for all models
            Entity.From = "Control To Display";
            Entity.To = "Audio Display Schedule";
            Entity.Entity_Type = "Self Speech"  ;
          }

             */
            
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            if(Entity.From.equals( "Control Voice Key" ) && Entity.Entity_Type.equals( "Self Speech"))return 0.0;
            return 0.0;
            
            
              
        }
        break;
        
      case dynamicevents:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
          case Beginning:

            Hashtable dynamic_items = sim.funs.TaskTemplateFun__Get_Current_Dynamic_Items();
            String event_name = Entity.Entity_Type;
            
            // predefined model setup: bst-learn sample 
            
            // bst-learn sample predefined model setup
            if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "sample_bst_learn_model_in_actr" ) ){
              switch (event_name){
                case "a_clicked" :{
                  Display_Item_Visual_Line line_current = (Display_Item_Visual_Line)dynamic_items.get("line_current") ;
                  Display_Item_Visual_Line line_target  = (Display_Item_Visual_Line)dynamic_items.get("line_target");
                  Display_Item_Visual_Line line_a       = (Display_Item_Visual_Line)dynamic_items.get("line_a") ;
                  if ( line_current.Length > line_target.Length){
                    line_current.End_Point_Screen_Location_X  -=  (int)line_a.Length;
                    line_current.Length -= line_a.Length;
                  }
                  else{
                    line_current.End_Point_Screen_Location_X  +=  (int)line_a.Length;
                    line_current.Length += line_a.Length;
                  }
                  sim.funs.TaskTemplateFun__Refresh_Dynamic_Item( line_current , 0.0F); //two parameters: item pointer, time delay before the refresh
                  break;
                }
                
                case "b_clicked" :{
                  Display_Item_Visual_Line line_current = (Display_Item_Visual_Line)dynamic_items.get("line_current") ;
                  Display_Item_Visual_Line line_target  = (Display_Item_Visual_Line)dynamic_items.get("line_target");
                  Display_Item_Visual_Line line_b       = (Display_Item_Visual_Line)dynamic_items.get("line_b") ;
                  if ( line_current.Length > line_target.Length){
                    line_current.End_Point_Screen_Location_X  -=  (int)line_b.Length;
                    line_current.Length -= line_b.Length;
                  }
                  else{
                    line_current.End_Point_Screen_Location_X  +=  (int)line_b.Length;
                    line_current.Length += line_b.Length;
                  }
                  sim.funs.TaskTemplateFun__Refresh_Dynamic_Item( line_current , 0.0F); //two parameters: item pointer, time delay before the refresh
                  break;
                }
                
                case "c_clicked" :{
                  Display_Item_Visual_Line line_current = (Display_Item_Visual_Line)dynamic_items.get("line_current") ;
                  Display_Item_Visual_Line line_target  = (Display_Item_Visual_Line)dynamic_items.get("line_target");
                  Display_Item_Visual_Line line_c       = (Display_Item_Visual_Line)dynamic_items.get("line_c") ;
                  if ( line_current.Length > line_target.Length){
                    line_current.End_Point_Screen_Location_X  -=  (int)line_c.Length;
                    line_current.Length -= line_c.Length;
                  }
                  else{
                    line_current.End_Point_Screen_Location_X  +=  (int)line_c.Length;
                    line_current.Length += line_c.Length;
                  }
                  sim.funs.TaskTemplateFun__Refresh_Dynamic_Item( line_current , 0.0F); //two parameters: item pointer, time delay before the refresh
                  break;
                }
                
                case "reset_clicked" :{
                  Display_Item_Visual_Line line_current = (Display_Item_Visual_Line)dynamic_items.get("line_current") ;
                  line_current.End_Point_Screen_Location_X  -=  (int)line_current.Length;
                  line_current.Length = 0.0F;
                  
                  sim.funs.TaskTemplateFun__Refresh_Dynamic_Item( line_current , 0.0F); //two parameters: item pointer, time delay before the refresh
                  break;
                }
                default:{
                  System.out.println("Dynamic Events Beginning Effect has undefine event_name: " + event_name);
                  break;
                }
              }
            } //end of bst-learn sample predefined model setup
            
            
            // predefined model setup: dual-task sample, typing_and_reading_comprehension
            
            // dual-task sample, typing_and_reading_comprehension predefined model setup
            else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "dual_task_sample_typing_and_reading_comprehension" ) ){
              switch (event_name){
                case "key_pressed" :{
                  sim.vars.sampleModelVar__transcript_typing_char_num_in_word++;  //also need to define task specific global variable (int).
                  String a_result_line = Double.toString(GlobalUtilities.round(SimSystem.clock() , 3)) + "\t" +  ((Response_Item_Key_Press)Entity.Response_Item).Key  + "\t" + sim.vars.sampleModelVar__transcript_typing_char_num_in_word;
                  sim.funs.ProgramUtilitiesFun__Output_Response_Results_Txt( a_result_line );
                  if(((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "space" ) || ((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "return"))sim.vars.sampleModelVar__transcript_typing_char_num_in_word= 0;
                  if(((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "return")) sim.funs.TaskTemplateFun__Terminate_Display_Response_Stage_Of_This_Trial(0.0F);
                  break;
                }
                default:{
                  System.out.println("Dynamic Events Beginning Effect has undefine event_name: " + event_name);
                  break;
                }
              }
            } //end of dual-task sample, typing_and_reading_comprehension model setup
            
            
            else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "touch_screen_typing" ) ){
            	
            	String tappedContent = event_name.substring(0, event_name.length() - "_tapped".length());
            	sim.vars.sampleModelVar__transcript_typing_char_num_in_word++;  //also need to define task specific global variable (int).
                String a_result_line = Double.toString(GlobalUtilities.round(SimSystem.clock() , 3)) + "\t" +  tappedContent  + "\t" + sim.vars.sampleModelVar__transcript_typing_char_num_in_word;
                sim.funs.ProgramUtilitiesFun__Output_Response_Results_Txt( a_result_line );
                if( tappedContent.equals( "send") )sim.vars.sampleModelVar__transcript_typing_char_num_in_word= 0;

              } //end of dual-task sample, typing_and_reading_comprehension model setup
            
            
            // predefined model setup: experiment_medical_decision_making 
            
            else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_medical_decision_making"  )){
       
              System.out.println("TODO dynamicevents, sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( experiment_medical_decision_making )");
              
 //TODO             
              
//              switch (event_name){
//                case "space" :{
//                  MedicalExp.FormManager.theMonitoringTask.spaceBarPressed();
//                  break;
//                }
//                case "d" :{
//                  MedicalExp.FormManager.theMemoryTask.dPressed();
//                  break;
//                }
//                case "f" :{
//                  MedicalExp.FormManager.theMemoryTask.fPressed();
//                  break;
//                }
//                case "sl_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.T1_Click_Model();
//                  break;
//                }
//                case "oe_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.T2_Click_Model();
//                  break;
//                }
//                case "br_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.T3_Click_Model();
//                  break;
//                }
//                case "1_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R1_Click_Model();
//                  break;
//                }
//                case "2_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R2_Click_Model();
//                  break;
//                }
//                case "3_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R3_Click_Model();
//                  break;
//                }
//                case "4_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R4_Click_Model();
//                  break;
//                }
//                case "6_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R6_Click_Model();
//                  break;
//                }
//                case "7_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R7_Click_Model();
//                  break;
//                }
//                case "8_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R8_Click_Model();
//                  break;
//                }
//                case "9_clicked":{
//                  MedicalExp.FormManager.theMedicalTask.R9_Click_Model();
//                  break;
//                }
//                
//                default:{
//                  System.out.println("Dynamic Events Beginning Effect has undefine event_name: " + event_name);
//                  break;
//                }
//              }
              
            } 
            
            
            
            
            else { // general customization
              
              switch (event_name){
                
                //Handlers of dynamic event triggers defined in task templates
                
                ////////////////////////////////////////////////////////////////////////////////////
                /// Write below for handlers of dynamic event triggers defined in task templates. //
                ////////////////////////////////////////////////////////////////////////////////////
                
                
                
                /*  
              // transcript typing task model
              //output customized results to response_results.txt
              case "key_pressed" :{
                Chunk command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable[((Response_Item_Key_Press)Entity.Response_Item).Key];
                String hand_finger = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "hand") + "\t" + sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "finger");
                SampleModelVar__transcript_typing_char_num_in_word++;
                String a_result_line = GlobalUtilities.round(SimSystem.clock(), 3).ToString() + "\t"  +  SampleModelVar__transcript_typing_char_num_in_word + "\t"  + ((Response_Item_Key_Press)Entity.Response_Item).Key  + "\t" + hand_finger  + "\t" + ((Response_Item_Key_Press)Entity.Response_Item).Note;
            //System.out.println("Dynamic Events, a_result_line: " + a_result_line);   
                sim.funs.ProgramUtilitiesFun__Output_Response_Results_Txt( a_result_line );
                if(((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "space") || ((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "return"))SampleModelVar__transcript_typing_char_num_in_word= 0;
                if(((Response_Item_Key_Press)Entity.Response_Item).Key.equals( "return")) sim.funs.TaskTemplateFun__Terminate_Display_Response_Stage_Of_This_Trial(0.0);
                break;
              }
                 */  
                
                
                ///////////////////////////////////////////////////////////////////////////
                /// End of handlers of dynamic event triggers defined in task templates. //
                ///////////////////////////////////////////////////////////////////////////
                /// 
                
                
                default:{
                  System.out.println("Dynamic Events Beginning Effect has undefine event_name: " + event_name);
                  break;
                }
              }
            }
            break;
            
          case Ending:
            
            Hashtable dynamic_items1 = sim.funs.TaskTemplateFun__Get_Current_Dynamic_Items();
            
            // predefined model setup: bst-learn sample 
            
            // bst-learn sample predefined model setup
            if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "sample_bst_learn_model_in_actr" ) ){
              // bst-learn sample
              Display_Item_Visual_Line line_current = (Display_Item_Visual_Line)dynamic_items1.get("line_current") ;
              Display_Item_Visual_Line line_target  = (Display_Item_Visual_Line)dynamic_items1.get("line_target");
              
              if (line_current.Length == line_target.Length){
                Display_Item_Visual_Text text_done = (Display_Item_Visual_Text)dynamic_items1.get("text_done");
                sim.funs.TaskTemplateFun__Refresh_Dynamic_Item( text_done , 0.0F); //two parameters: item pointer, time delay before the refresh 
                sim.funs.TaskTemplateFun__Terminate_Display_Response_Stage_Of_This_Trial( 10.0F ); //one papameter: time delay
              }
            }
            
            
            // Handlers of any other dynamic events without pre-defined triggers
            
            ///////////////////////////////////////////////
            /// Write below for any other dynamic events //
            ///////////////////////////////////////////////
            
            
            
            
            
            
            //////////////////////////////////////
            /// End of any other dynamic events //
            //////////////////////////////////////
            break;
            
          case Timing:
            return 0.0;
            
              
        }

        
      case controlvoicekey:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
          case Beginning:
            
           
            Entity.Time_Computed = false;
            break;
            
          case Ending:
            if (Entity.From.equals( "Speech Execution") && Entity.To.equals( "Control Voice Key" ) && Entity.Entity_Type.equals( "Speech Execution Result")){
              String output = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "string");
              sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "SPEECH" + "\t\t" + "OUTPUT-SPEECH" + " " + output); 
              Entity.From = "Control Voice Key";
              Entity.To = "Control To Display";
              Entity.Entity_Type = "Self Speech"  ;
              
              //record speech response here:
              //aa_Altmann_Study1_Record_Voice_Response(output);
              Response_Item_Speech a_response = new Response_Item_Speech();
              a_response.Speech = output;
              a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
              Entity.Response_Item = a_response;
              
              sim.funs.AnimatorModuleFun__Produce_Speech( output ); 
              //System.out.println("break point Control Voice Key"); 
            }
            
            break;
            
          case Timing:
            Entity.Time_Computed = true;
            return 0.0; //currently ACT-R speech execution time is 0.0;
            
            
              
        }
        break;
        
      case controlmotorreleasequeue:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
            
            
          case Beginning:
            // no beginning
            break;
            
          case Ending:
            
            //ignore trashed entity
            if(Entity.Trash == false){
              
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                
                
                case "world3d-sp-driving-control-manual-joystick":{
                  String axis = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "axis");
                  double value = 0.0;
                  switch(axis){
                    case "x":{
                      sim.vars.deviceModule.Joystick_X = (double) value;
                      break;
                    }
                    case "y":{
                      sim.vars.deviceModule.Joystick_Y = (double) value;
                      break;
                    }
                    case "z":{
                      sim.vars.deviceModule.Joystick_Z = (double) value;
                      break;
                    }
                    case "pitch":{
                      sim.vars.deviceModule.Joystick_Pitch = (double) value;
                      break;
                    }
                    case "yaw":{
                      sim.vars.deviceModule.Joystick_Yaw = (double) value;
                      break;
                    }
                    case "roll":{
                      sim.vars.deviceModule.Joystick_Roll = (double) value;
                      break;
                    }
                    default:{
                      System.out.println("Error! Control Motor has undefined axis: " + axis);
                      SimSystem.abort();
                      break;  
                    }
                  }
                  
                  
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "world3d-sp-driving-control-manual-joystick action axis: " + axis + " released. ");
                  
                  break;
                }
                
                
                default:{
                  System.out.println("Error! Control Motor Release Queue has undefined case request_type: " + request_type);
                  SimSystem.abort();
                  break;  
                }
              }
              
            }
            
            break;
            
          case Timing:
            
            if(Entity.Trash == false){
              
              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                
                
                case "world3d-sp-driving-control-manual-joystick":{
                  String duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "duration" );
                  return Double.parseDouble( duration ) ; 
                  
                  
                }
                
                
                default:{
                  System.out.println("Error! Control Motor Release Queue timinig has undefined case request_type: " + request_type);
                  SimSystem.abort();
                  return 0.0;
                    
                }
              }
              
            }
            
            return 0.0;
            
              
        }
        break;
                
      case delayedfunctioncallnoreturnvalue:
        
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            //used by sim.funs.ProgramUtilitiesFun__Delayed_Function_Call_No_Return_Value to issue a delayed function call
            
          case Beginning:
            //System.out.println("Delayed_Function_Call_No_Return_Value incomming");
            break;
            
          case Ending:
            
            //System.out.println("Delayed_Function_Call_No_Return_Value out going");
            Object the_parameters = Entity.Response_Item;
            
            //System.out.println("TODO delayedfunctioncallnoreturnvalue, Animator3D, PluginFunctions, Sound_Plugin");
            
            switch(Entity.Entity_Type){
              
//TODO              
//              case "Animator3D_Hide":{
//                Animator3D.Hide((String)the_parameters);
//                break;
//              }
//              case "Open_RT_Window":{
//                PluginFunctions.Experiment_Specifics_Open_RT_Window((String)the_parameters );
//                break;
//              }
//              case "Close_RT_Window":{
//                PluginFunctions.Experiment_Specifics_Close_RT_Window((String)the_parameters );
//                break;
//              }
//              
              case "TaskTemplateFun__Update_DriverCar_Accelbrake":{
                sim.funs.TaskTemplateFun__Update_DriverCar_Accelbrake( (double)the_parameters );
                break;
              }
              
//              case "VocalizeSentence":{
//                Sound_Plugin.SetSpeechRate(0);
//                Sound_Plugin.TextToSpeech((String)the_parameters );
//                break;
//              }
//              
//              case "Plugin_MedicalDecisionTask_TrailDoneAfterFeedback":
//              {
//                MedicalExp.FormManager.theMedicalTask.trailDoneAfterFeedback();
//                break;  
//              }
//              case "Plugin_MemoryTask_StartSubTrial":
//              {
//                MedicalExp.FormManager.theMemoryTask.startSubTrial_model();
//                
//                break;  
//              }
              default:{
                System.out.println("Error! Delayed_Function_Call_No_Return_Value has undefined function name: " + Entity.Entity_Type);
                SimSystem.abort();
                break;
              }
            }
            
            Entity.Response_Item = null;
            
            
            break;
            
          case Timing:
            if( GlobalUtilities.round(SimSystem.clock(),3 ) > GlobalUtilities.round(Entity.Scheduled_Task_Enter_Clock_Time ,3 )) { //otherwise, if the Scheduled time was just assigned with the Clock time, the new Clock time, though is still the same, will be greater than the scheduled time
              return 0.0;
            }
            else {
              return GlobalUtilities.round((Entity.Scheduled_Task_Enter_Clock_Time - SimSystem.clock()),3 );
            }
 
        }
        break;
        
        
      case controlmotor:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;
            
          case Beginning:
            //Entity.Time = 0.0;
            Entity.Time_Computed = false;
            //if(Clock >= 0.5)System.out.println("Task 221 " + Entity.Time);
            
            // special case for if chunk isa move-cursor and has incremental-movments
            // TODO check for ply chunk type?
            if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && Entity.Chunk.Slot.containsKey("incremental-movements")) {
              // check if this is the first time it has come through
              if(!Entity.Chunk.Slot.containsKey("step")) {
                // set current movement to 0
                // TOD usually these are only strings
                Object n = Entity.Chunk.Slot.get("step");
                n = 0;
              }
            }
            
            

            break;
            
          case Ending:
            
            //here the device receives the (start of) Control Motor event
            //if(Clock >= 0.5) System.out.println("Task 221 " + Entity.Time);
            if (Entity.From.equals( "Motor Execution") && Entity.To.equals( "Control Motor") && Entity.Entity_Type.equals( "Motor Execution Result")){
              //System.out.println(Clock + " Initiated: " + sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request );
              
              //put experiment Control Motor result collection here

              String request_type = Entity.Chunk.Chunk_Type;
              switch(request_type){
                case "press-key":{ 
                  String key = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "key");
                  
                  // Typing Key Errors
                  String note = "";
                  
                  //for motor subnetwork time
                  if (!Entity.Note.equals( "" )){
                    note = Entity.Note;
                    Entity.Note = "";
                  }
                  
                  
                  
                  if (note.equals( "" ) &&  Entity.Note.equals( "motor_subnetwork_decay" )){ 
                    //System.out.println("motor_subnetwork_decay2" );
                    note = "Omission_Error_Due_To_Motor_Subnetwork_Decay";
                    Entity.Note = "";
                  }
                  
                  if(note.equals( "" ) && sim.vars.motorModule.Enable_Typing_Key_Omission_Due_To_Little_Force){
                    Chunk command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.get(key);
                    String hand_finger = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "hand") + "_" + sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "finger");
                    double threshold = 0.0;
                    switch(hand_finger){
                      case "right_pinkie":{
                        threshold = 0.0035628804826039;
                        break;
                      }
                      case "right_ring":{
                        threshold = 0.0000700476365953;
                        break;
                      }
                      case "right_middle":{
                        threshold = 0.0000000006264101;
                        break;
                      }
                      case "right_index":{
                        threshold = 0.0000000001143315;
                        break;
                      }
                      case "right_thumb":{
                        threshold = 0.0;
                        break;
                      }
                      
                      case "left_pinkie":{
                        threshold = 0.0000147185523893;
                        break;
                      }
                      case "left_ring":{
                        threshold = 0.0000000000000005;
                        break;
                      }
                      case "left_middle":{
                        threshold = 0.0000018173234198;
                        break;
                      }
                      case "left_index":{
                        threshold = 0.0000000000042882;
                        break;
                      }
                      case "left_thumb":{
                        threshold = 0.0;
                        break;
                      } 
                      default:{
                        System.out.println("WARNING! Control Motor has undefined hand_finger: " + hand_finger);
                        break;
                      }
                    }
                    
                    double rand = Math.random();
                    if ( rand < threshold ) note = "Omission_Error_Due_To_LackOfForce";
                  }
                  
                  if(note.equals( "" ) && sim.vars.motorModule.Enable_Typing_Key_Error_Due_To_Distance_Distribution){
                    //check finger movement distance
                    Chunk command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.get(key);
                    String command_type = command.Chunk_Type;
                    if( command_type.equals( "peck-recoil")){ //not home row key, consider distance error
                      String r = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "r");
                      double r_double = Double.parseDouble(r);
                      double g = 0.55; //cm, gap between two adjacent keys
                      double k = 0.65; //cm, half key width
                      double f = 0.65; //cm, finger-key contact diameter
                      double target_distance = r_double * (g + 2 * k) ;
                      Random random = new Random();
                      double dis_error = random.nextGaussian()*0.317; //cm , Wu and Liu 2008, equation 20. page 6:33
                      double dis_error_abs = Math.abs( dis_error );
                      if (r_double == 1 || r_double == 2){ //Wu and Liu 2008, page 6:22
                        if ( dis_error_abs < k+(g-f/2)){
                          //do nothing, correct hit 
                        }
                        else if (dis_error_abs < k + f/2) { // >= k+(g-f/2) by default here
                          note = "\t" + "Intrusion_Error_Due_To_Distance_Distribution" + ",r=" + r_double + ",dis_error" + dis_error + "(cm)";
                        }
                        else { //>k+f/2
                          note = "\t" + "Substitution_Error_Due_To_Distance_Distribution"+ ",r=" + r_double + ",dis_error" + dis_error + "(cm)";  
                        }
                      }
                      else if (r_double == 1.41 || r_double == 2.24){ 
                        if ( dis_error_abs < 1.41*(k+(g-f/2)) ){
                          //do nothing, correct hit 
                        }
                        else if (dis_error_abs < 1.41*(k + f/2) ) { 
                          note = "\t" + "Intrusion_Error_Due_To_Distance_Distribution"+ ",r=" + r_double + ",dis_error" + dis_error + "(cm)";
                        }
                        else { 
                          note = "\t" + "Substitution_Error_Due_To_Distance_Distribution"+ ",r=" + r_double + ",dis_error" + dis_error + "(cm)";  
                        }
                      }
                      else System.out.println("Error! Control Motor ending effect has undefined r_double: " + r_double);
                    }
                    else{ //at home row keys,
                      // do nothing, no distance error  
                    }
                  }
                  
                  
                  
                  Two_Tuple temp_two_tuple = (Two_Tuple) sim.vars.deviceModule.Key_To_Location_Hashtable.get(key);
                  String output = "#(" + temp_two_tuple.Ob1.toString() + " " + temp_two_tuple.Ob2.toString() + ")";
                  
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "OUTPUT-KEY" + " " + output +  " " + key + " " + note); 
                  
                  //record keyboard response here:
                  Response_Item_Key_Press a_response = new Response_Item_Key_Press();
                  a_response.Key = key;
                  a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                  a_response.Note = note;
                  Entity.Response_Item = a_response;
                  
                  //prepare for manual response visualization
                  if(sim.vars.animatorModule.Show_Animator){
                    Chunk command = (Chunk) sim.vars.deviceModule.Press_Key_To_Motor_Command_Hashtable.get(key);
                    String hand_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "hand");
                    String finger_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(command, "finger");
                    sim.funs.AnimatorModuleFun__Set_Finger_Press_On_A_Place(hand_string, finger_string, key ); 
                  }
                  
                  break;
                }
                case "punch": { //Entity.Chunk is the command
                  
                  String hand_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                  String finger_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finger");
                  Two_Tuple finger_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand_string, finger_string );
                  int key_punched_x =(int) finger_location.Ob1;
                  int key_punched_y =(int) finger_location.Ob2;
                  /* //obsolete
                Motor_Module_Hand hand_ob = new Motor_Module_Hand();
                if(hand_string.toLowerCase().equals( "right" )) hand_ob = sim.vars.motorModule.Right_Hand;
                else if (hand_string.toLowerCase().equals( "left")) hand_ob = sim.vars.motorModule.Left_Hand;
                else {
                  System.out.println ("Control Motor punch has hand_string error: " + hand_string);
                  break;
                }
                int hand_x = (int) hand_ob.Location.Ob1;
                int hand_y = (int) hand_ob.Location.Ob2;
                int finger_offset_x = (int) ((Two_Tuple)hand_ob.Finger_Location_Offsets[finger_string]).Ob1;
                int finger_offset_y = (int) ((Two_Tuple)hand_ob.Finger_Location_Offsets[finger_string]).Ob2;
                int key_punched_x = hand_x + finger_offset_x;
                int key_punched_y = hand_y + finger_offset_y;
                   */
                  
                  String key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key (key_punched_x, key_punched_y);
                  String output = "#(" + Integer.toString(key_punched_x) + " " + Integer.toString(key_punched_y) + ")";
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "OUTPUT-KEY" + " " + output + ", " + key); 
                  
                  //record keyboard response here:
                  //aa_Altmann_Study1_Record_Keyboard_Response(key, finger_string);
                  //record keyboard response here:
                  Response_Item_Key_Press a_response = new Response_Item_Key_Press();
                  a_response.Key = key;
                  a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                  Entity.Response_Item = a_response;
                  
                  //prepare for manual response visualization
                  if(sim.vars.animatorModule.Show_Animator){
                    sim.funs.AnimatorModuleFun__Set_Finger_Press_On_A_Place(hand_string, finger_string, key ); 
                  }
                  
                  break;
                }
                
                case "tap": { //Entity.Chunk is the command
                    // here, does not consider which finger is tapping, use the hand location XY for the response location.
                	
                    String hand_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "hand");
                    String finger_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "finger");
                    
                    Motor_Module_Hand hand = sim.funs.MotorModuleFun__Get_Hand_From_Hand_String(hand_string);
                	int hand_x = (int)hand.Location.Ob1;
                    int hand_y = (int)hand.Location.Ob2;
                    String output = "at screen pixel location (" + hand_x + " " + hand_y + ")";
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "TAP" + " " + output); 
                    
                    Response_Item_Key_Press a_response = new Response_Item_Key_Press();
                    a_response.Mouse_Cursor_X =  hand_x;
                    a_response.Mouse_Cursor_Y = hand_y;
                    a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                    a_response.Key = "tap";
                    a_response.Note = "tap";
                    Entity.Response_Item = a_response;
                    
                    break;
                  }
                
                case "click-mouse": { //Clicking the mouse is really just a punch with the right index finger. ACT-R 6.0 motor.lisp
                  String hand_string = "right";
                  String finger_string = "index";
                  Two_Tuple finger_location = sim.funs.MotorModuleFun__Get_Finger_Resting_Location( hand_string, finger_string );
                  int key_punched_x =(int) finger_location.Ob1;
                  int key_punched_y =(int) finger_location.Ob2;
                  
                  if( !sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals( "mouse" )) { 
                    System.out.println( " #|Warning: CLICK-MOUSE requested when hand not at mouse #(28, 2)! |# " );
                    Entity.Trash = true;
                    break;
                  }
                  
                  String key = sim.funs.DeviceModuleFun__Keyboard_Location_To_Key (key_punched_x, key_punched_y);
                  String output = "#(" + Integer.toString(key_punched_x) + " " + Integer.toString(key_punched_y) + ")";
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "OUTPUT-KEY" + " " + output + ", " + key); 
                  
                  //record response here:
                  Response_Item_Key_Press a_response = new Response_Item_Key_Press();
                  a_response.Key = key;
                  a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                  Entity.Response_Item = a_response;
                  
                  //prepare for manual response visualization
                  if(sim.vars.animatorModule.Show_Animator){
                    sim.funs.AnimatorModuleFun__Set_Finger_Press_On_A_Place(hand_string, finger_string, key ); 
                  }
                  
                  break;
                }
                case "move-cursor": {
                  if(! sim.vars.motorModule.Right_Hand.Object_Type_In_Hand.equals( "mouse" )) { 
                    System.out.println( " #|Warning: move-cursor requested when hand not at mouse #(28, 2)! |# " );
                    Entity.Trash = true;
                    break;
                  }
                  int old_cursor_x = sim.vars.deviceModule.Mouse_Cursor_Screen_X;
                  int old_cursor_y = sim.vars.deviceModule.Mouse_Cursor_Screen_Y;
                  
                  int new_cursor_x, new_cursor_y, target_loc_x_in_pixel = 0, target_loc_y_in_pixel = 0;
                  // if incremental, get move location from chunk
                  if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && Entity.Chunk.Slot.containsKey("incremental-movements")) {
                    
                    System.out.println("TODO Server logics controlmotor, move-cursor need incremental-movements mechanism");
//TODO                    
//                    new_cursor_x = (int)(Entity.Chunk.Slot.get("incremental-movements") as List<Two_Tuple>)[(int)Entity.Chunk.Slot.get("step")].Ob1;
//                    new_cursor_y = (int)(Entity.Chunk.Slot["incremental-movements"] as List<Two_Tuple>)[(int)Entity.Chunk.Slot.get("step")].Ob2;
                    
                    //temp for compiler TODO
                    new_cursor_x = 0;
                    new_cursor_y = 0;
                    
                  } else {
                    
                    //these values should have been prepared by sim.funs.MotorModuleFun__Compute_Execution_Time
                    //ChunkFun__Popout_Message_Show_Chunk_Contents( Entity.Chunk );
                    target_loc_x_in_pixel = Integer.parseInt( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "target_loc_x_in_pixel") );
                    target_loc_y_in_pixel = Integer.parseInt( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "target_loc_y_in_pixel") );
                    double approch_w_in_visual_angle_degree = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "approaching_width_in_visual_angle_degree" ) );
                    Two_Tuple new_cursor_loc = sim.funs.MotorModuleFun__Add_Move_Cursor_Noise ( target_loc_x_in_pixel,  target_loc_y_in_pixel, (double) approch_w_in_visual_angle_degree );
                    
                    new_cursor_x = (int) new_cursor_loc.Ob1;
                    new_cursor_y = (int) new_cursor_loc.Ob2;
                  }
                  
                  String with_noise = " without noise";
                  if( sim.vars.motorModule.Cursor_Noise ) with_noise = " with Noise. It is (" + target_loc_x_in_pixel + ", " + target_loc_y_in_pixel + ") before noise added.";
                  
                  //output trace
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "MOVE-CURSOR to (" + new_cursor_x + ", " + new_cursor_y + ")" + with_noise );
                  
                  //update cursor location
                  sim.vars.deviceModule.Mouse_Cursor_Screen_X = new_cursor_x;
                  sim.vars.deviceModule.Mouse_Cursor_Screen_Y = new_cursor_y;
                  
                  //record response here:
                  Response_Item_Move_Cursor a_response = new Response_Item_Move_Cursor();
                  a_response.Start_X = old_cursor_x;
                  a_response.Start_Y = old_cursor_y;
                  a_response.End_X = new_cursor_x;
                  a_response.End_Y = new_cursor_y;
                  a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                  double motor_execution_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_execution_time") );
                  a_response.Duration = (double) motor_execution_time ;
                  Entity.Response_Item = a_response;
                  
                  //prepare cursor visualization
                  if(sim.vars.animatorModule.Show_Animator && QnactrSimulation.taskVisualization2DEnable){
                    
                    sim.vars.taskVisualization2D.showObject(sim.vars.taskVisualization2D.mouseCursorID); //Animator.ShowImage ("501");  Animator.MoveImageToTop("501");
                    sim.vars.taskVisualization2D.setDynamicObjectLocation(sim.vars.taskVisualization2D.mouseCursorID, new_cursor_x, new_cursor_y); // Animator.PlaceImage("501", new_cursor_x, new_cursor_y); //this is better than MoveImage, which needs too long time to be able shown during the simulation.
                    
                    //  System.out.println("Control Motor" + new_cursor_x + " " + new_cursor_y +  " " + motor_execution_time);
                  }
                  
                  break;
                }
                case "move-hand-touch": {       
                	
                	String handString = sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "hand");
                	//System.out.println("handString: " + handString );
                	Motor_Module_Hand hand = sim.funs.MotorModuleFun__Get_Hand_From_Hand_String(handString);
                	int old_hand_x = (int)hand.Location.Ob1;
                    int old_hand_y = (int)hand.Location.Ob2;
                    int new_hand_x, new_hand_y, target_loc_x_in_pixel = 0, target_loc_y_in_pixel = 0;
                    // if incremental, get move location from chunk
                    if(Entity.Chunk.Chunk_Type.equals( "move-hand-touch") && Entity.Chunk.Slot.containsKey("incremental-movements")) {
                      
                      System.out.println("TODO Server logics controlmotor, move-hand-touch need incremental-movements mechanism");
  //TODO                    
//                      new_hand_x = (int)(Entity.Chunk.Slot.get("incremental-movements") as List<Two_Tuple>)[(int)Entity.Chunk.Slot.get("step")].Ob1;
//                      new_hand_y = (int)(Entity.Chunk.Slot["incremental-movements"] as List<Two_Tuple>)[(int)Entity.Chunk.Slot.get("step")].Ob2;
                      
                      //temp for compiler TODO
                      new_hand_x = 0;
                      new_hand_y = 0;
                      
                    } else {
                      
                      //these values should have been prepared by sim.funs.MotorModuleFun__Compute_Execution_Time
                      //ChunkFun__Popout_Message_Show_Chunk_Contents( Entity.Chunk );
                      target_loc_x_in_pixel = Integer.parseInt( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "target_loc_x_in_pixel") );
                      target_loc_y_in_pixel = Integer.parseInt( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "target_loc_y_in_pixel") );
                      double approch_w_in_visual_angle_degree = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "approaching_width_in_visual_angle_degree" ) );
                      
                      //here assume move-hand-touch uses the same noise as cursor noise
                      Two_Tuple new_hand_loc = sim.funs.MotorModuleFun__Add_Move_Cursor_Noise ( target_loc_x_in_pixel,  target_loc_y_in_pixel, (double) approch_w_in_visual_angle_degree );
                      
                      new_hand_x = (int) new_hand_loc.Ob1;
                      new_hand_y = (int) new_hand_loc.Ob2;
                    }
                    
                    String with_noise = " without noise";
                    if( sim.vars.motorModule.Cursor_Noise ) with_noise = " with Noise. It is (" + target_loc_x_in_pixel + ", " + target_loc_y_in_pixel + ") before noise added.";
                    
                    //output trace
                    sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "MOVE-HAND-TOUTH to (" + new_hand_x + ", " + new_hand_y + ")" + with_noise );
                    
                    //update hand location
                    hand.Location.Ob1 = new_hand_x;
                    hand.Location.Ob2 = new_hand_y;
                    
                    //record response here:
                    Response_Item_Move_Hand_Touch a_response = new Response_Item_Move_Hand_Touch();
                    a_response.Start_X = old_hand_x;
                    a_response.Start_Y = old_hand_y;
                    a_response.End_X = new_hand_x;
                    a_response.End_Y = new_hand_y;
                    a_response.Clock_Time = GlobalUtilities.round(SimSystem.clock(),3);
                    double motor_execution_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_execution_time") );
                    a_response.Duration = (double) motor_execution_time ;
                    Entity.Response_Item = a_response;
                    
                    //prepare hand visualization
                    if(sim.vars.animatorModule.Show_Animator && QnactrSimulation.taskVisualization2DEnable){
                      
                    	//TODO, change picture to a left/right hand
                    	//currently, use mouse cursor picture
                    	
                      sim.vars.taskVisualization2D.showObject(sim.vars.taskVisualization2D.mouseCursorID); //Animator.ShowImage ("501");  Animator.MoveImageToTop("501");
                      sim.vars.taskVisualization2D.setDynamicObjectLocation(sim.vars.taskVisualization2D.mouseCursorID, new_hand_x, new_hand_y); // Animator.PlaceImage("501", new_cursor_x, new_cursor_y); //this is better than MoveImage, which needs too long time to be able shown during the simulation.
                      
                      //  System.out.println("Control Motor" + new_cursor_x + " " + new_cursor_y +  " " + motor_execution_time);
                    }
                    
                    break;
                  }
                case "world3d-driving-two-point-visual-manual-steer":{
                  //update DriveCar steer and wheel
                  double delta_steer_degree = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "delta-steer") );
                  //System.out.println(delta_steer_degree);
                  sim.funs.TaskTemplateFun__Update_DriverCar_Steer_And_Wheel_With_Delta_Steer((double) delta_steer_degree);
                  
                  //output trace
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "world3d-driving-two-point-visual-manual-steer turned the steering wheel for: "  + delta_steer_degree + " (degree)"  );
                  
                  Entity.Trash = true; //do not need to send this to "Control To Display", instead, world3D refresh will change the world3D and animator3D.
                  break;
                }
                case "customized-manual-action":{
                  String name = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "name" );
                  
                  switch(name){ 
                    case "space-1d-speed-control":{
                      World3D_Template_Driving_Method method_pointer = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
                      method_pointer.sim = sim;
                      double  delta_speed = method_pointer.Get_Space_1D_Direct_Speed_Control_Delta_Speed( Entity.Chunk );
                      
                      World3D_DriverCar driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
                      driver_car.Vehicle_Basic.Speed += delta_speed;
                      
                      //output trace
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "customized-manual-action with name: " + name  + " action done with speed change: " + delta_speed + " (m/s)");
                      Entity.Trash = true; //do not need to send this to "Control To Display",  
                      break;
                    }
                    case "right-foot-press":{
                      
                      //output trace
                      sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "customized-manual-action with name: " + name  + " action done");
                      sim.funs.ProgramUtilitiesFun__Output_Foot_Results_Txt (Double.toString(GlobalUtilities.round(SimSystem.clock() ,3)) + "\t" + name);
                      Entity.Trash = true; //do not need to send this to "Control To Display",  
                      break;
                    }
                    default:{
                      System.out.println("Error! Control Motor has undefined name: " + name + " for customized-manual-action");
                      SimSystem.abort();
                      break;
                    }
                  }
                  
                  
                  
                  break;
                } //end of custom action
                
                
                
                case "world3d-sp-driving-control-manual-joystick":{
                  
                  //1, immediately update the dimension into the new value
                  String axis = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "axis");
                  String value_string = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "value");
                  if ( ! sim.funs.ProgramUtilitiesFun__Is_String_Double(value_string) ) {
                    System.out.println("Error! Control Motor has ! sim.funs.ProgramUtilitiesFun__Is_String_Double(value_string), value_string: " + value_string );
                    SimSystem.abort();
                  }
                  double value = Double.parseDouble (value_string);
                  //String duration = sim.funs.ChunkFun__Get_Chunk_Slot_Value(Entity.Chunk, "duration" );
                  
                  switch(axis){
                    case "x":{
                      sim.vars.deviceModule.Joystick_X = (double) value;
                      break;
                    }
                    case "y":{
                      sim.vars.deviceModule.Joystick_Y = (double) value;
                      break;
                    }
                    case "z":{
                      sim.vars.deviceModule.Joystick_Z = (double) value;
                      break;
                    }
                    case "pitch":{
                      sim.vars.deviceModule.Joystick_Pitch = (double) value;
                      break;
                    }
                    case "yaw":{
                      sim.vars.deviceModule.Joystick_Yaw = (double) value;
                      break;
                    }
                    case "roll":{
                      sim.vars.deviceModule.Joystick_Roll = (double) value;
                      break;
                    }
                    default:{
                      System.out.println("Error! Control Motor has undefined axis: " + axis);
                      SimSystem.abort();
                      break;  
                    }
                  }
                  
                  //2, handle when the press will be released. 
                  sim.funs.MotorModuleFun__Add_Item_To_Control_Motor_Release_Queue(request_type, Entity.Chunk);
                  
                  
                  //3, output trace
                  sim.funs.ProgramUtilitiesFun__Output_Trace_Txt("\t" + GlobalUtilities.round (SimSystem.clock(),3) + "\t" + "MOTOR" + "\t\t" + "world3d-sp-driving-control-manual-joystick action with axis: " + axis  + ", value: " + value + ". ");
                  Entity.Trash = true; //do not need to send this to "Control To Display",  
                  
                  break;
                }
                
                
                
                case "":{
                  System.out.println("Control Motor ending effect has an empty ISA command type.");
                  break;
                }
                default: {
                  System.out.println("Control Motor ending effect has undefine case: " + request_type);
                  break;
                } 
              }
              Entity.From = "Control Motor";
              Entity.To = "Control To Display";
              Entity.Entity_Type = "" ; //currently do not need a type specification
              
              // if incremental move and we're not done, prepare for loop
              if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && Entity.Chunk.Slot.containsKey("incremental-movements")){
                
                System.out.println("TODO Server logics controlmotor, world3d-sp-driving-control-manual-joystick need incremental-movements mechanism");
//TODO                
//                if( Integer.valueOf(Entity.Chunk.Slot.get("step")) < ((Entity.Chunk.Slot["incremental-movements"] as List<Two_Tuple>).size() - 1)) {
//                  // increment step
//                  Object r = Entity.Chunk.Slot.get("step");
//                  r = Integer.valueOf(Entity.Chunk.Slot.get("step")) + 1;
//                } 
//                else {
//                // if doing incremental but we're done, just kill the entity
//                Entity.Trash = true;
//                }
//                
                
              } else {
                Entity.From = "Control Motor";
                Entity.To = "Control To Display";
                Entity.Entity_Type = "" ; //currently do not need a type specification
                
                if(sim.vars.motorModule.Motor_Output_Sequence_Required){
                  if( sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request.equals(sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk))){
                    sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request = "";
                    
                    //send a JMT ACK event to motor initiation queue section to activate any queued entities due to limited motor resources
                    sim.funs.sendAckToQueue("Motor Initiation", 0.0); // this is needed for the is_motor_output_sequence_ok test in motor initiation release condition to work.
                    
                    
                  }
                  else System.out.println(SimSystem.getClock() + " WARNING! Control Motor has sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk) not equal to sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request. Entity.Chunk: " + sim.funs.ChunkFun__Get_Chunk_Contents(Entity.Chunk) + ", vs. Initiated_But_Output_Not_Finished_Request: \t" + sim.vars.motorModule.Initiated_But_Output_Not_Finished_Request);
                }
              }
            }
            
            break;
            
          case Timing:
            // if incremental mouse move, return incremental time
            if(Entity.Chunk.Chunk_Type.equals( "move-cursor") && Entity.Chunk.Slot.containsKey("incremental-movements")) {
              Entity.Time_Computed = true;
              return Double.valueOf(Entity.Chunk.Slot.get("steptime"));
            }
            //after this time, the control device can receive the event of a key press for example
            double motor_execution_time = Double.parseDouble( sim.funs.ChunkFun__Get_Chunk_Slot_Value( Entity.Chunk, "motor_execution_time") );
            Entity.Time_Computed = true;
            return motor_execution_time;
            
        }
        break;
                
        
      case world3dcyclicrefresh:
        switch (ServiceStage){
          case Release:
            return (sim.funs.getNumberOfQnactrEntityInServer(ServerName.toString()) < sim.funs.getServerCapacity(ServerName.toString())); //true;

            
          case Beginning:
                        

        	  if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_opends" )){
        		  //not sure, leave it blank as for torcs
                	
              } //end of model_drive_opends
        	  else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_torcs" )){
        		  //do not need to output results here, because results are available in TORCS output      
                	
              } //end of model_drive_torcs
        	  else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "unity_tangtang_2015" )){
        		  // may want to output some results. TODO
              } 
        	  else{ //old method, use internal 3D mo
        	  
	            //System.out.println("World3D_Cyclic_Refresh test");
	            //output first row titles for human drive results
	            if(SimSystem.clock() == 0.0 ){
	              String[] titles;
	              World3D_Template_Driving_Method the_method_2 = null;
	              if(sim.vars.world3DTemplate.Method_Object != null && sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) the_method_2 = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
	              sim.vars.programGlobalVar__Who_Drive = the_method_2.Who_Drive;
	              //sp-driv   
	              if(the_method_2 != null && (the_method_2.Who_Drive.toLowerCase().equals( "model-6dof-speed") || the_method_2.Who_Drive.toLowerCase().equals( "human-6dof-speed" ) ) ){ 
	                titles = new String[] {
	                    "Clock_Time(s)", 
	                    "Delta_X(cm)", "Delta_Y(cm)", "Delta_Z(cm)", 
	                    "Delta_Pitch(deg)", "Delta_Yaw(deg)", "Delta_Roll(deg)", 
	                    "Global_Speed_X(cm/s)", "Global_Speed_Y(cm/s)", "Global_Speed_Z(cm/s)", 
	                    "Global_Speed_Pitch(deg/s)", "Global_Speed_Yaw(deg/s)", "Global_Speed_Roll(deg/s)", 
	                    "Joystick_X", "Joystick_Y", "Joystick_Z", 
	                    "Joystick_Pitch", "Joystick_Yaw", "Joystick_Roll"
	                };
	              }
	              else{
	                
	                //experiment_driving_and_comprehension    
	                if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_driving_and_comprehension")){
	                 
	                  System.out.println("TODO Server logics world3dcyclicrefresh, experiment_driving_and_comprehension need PluginFunctions mechanism");
	//TODO           
	//                  String language = PluginFunctions.ExperimentSpecifics_Language; 
	//                  
	//                  String LeftButtonMean = PluginFunctions.GetExperimentSpecificsTrialDetailsParameterValue(PluginFunctions.ExperimentSpecifics_SubjectNum.ToString(), "1", "LeftButtonMean"); //all search for session 1, because all sessions have the same key assignment.
	//                  String key_instruction = "";
	//                  if (LeftButtonMean.equals( "Yes") && language.equals( "CH")) key_instruction = "左边的键代表\"相同\"; 右边的键代表\"不同\"";
	//                  else if (LeftButtonMean.equals( "No") && language.equals( "CH")) key_instruction = "左边的键代表\"不同\"; 右边的键代表\"相同\"";
	//                  else if (LeftButtonMean.equals( "Yes") && language.equals( "EN")) key_instruction = "The left key is for \"same\"; the right key is for \"different\"";
	//                  else if (LeftButtonMean.equals( "No") && language.equals( "EN")) key_instruction = "The left key is for \"different\"; the right key is for \"same\"";
	//                  
	//                  System.out.println(key_instruction + "\n\n" + "Close Output window.\n(Message from World3D_Cyclic_Refresh beginning. " + the_method_2.Who_Drive.toLowerCase() + ")" );
	                }
	                
	                else System.out.println("World3D_Cyclic_Refresh beginning: " + the_method_2.Who_Drive.toLowerCase());
	                
	                titles = new String[] {"Clock_Time(s)", "Distance_On_Road(m)", "Lateral_Distance(m)", "Speed(m/s)", "Head_Angle_Between_Road_Direction(degreeRight+)", "Steer(degreeRight+)", "Accelerator(0-1)", "Brake(0-1)"};
	              }
	              sim.funs.ProgramUtilitiesFun__Output_Human_Drive_Results_Txt(  sim.funs.ProgramUtilitiesFun__StringArray_To_String_Show_Empty(titles) );
	            }
	            
	            //output Human_Drive_Results
	            if( sim.vars.world3DTemplate.Method_Object != null && (sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method)){
	              World3D_DriverCar the_driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
	              String[] a_line_of_human_or_model_driving_results;
	              // sp-driv  
	              if(sim.vars.programGlobalVar__Who_Drive.toLowerCase().equals( "model-6dof-speed") || sim.vars.programGlobalVar__Who_Drive.toLowerCase().equals( "human-6dof-speed" )){
	                double Delta_X = (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob1 - the_driver_car.Vehicle_Basic.Loc_X;
	                double Delta_Y = (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob2 - the_driver_car.Vehicle_Basic.Loc_Y;
	                double Delta_Z = (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob3 - the_driver_car.Vehicle_Basic.Loc_Z;
	                double Delta_Pitch = (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob4 - the_driver_car.Vehicle_Basic.Pitch;
	                double Delta_Yaw =   (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob5 - the_driver_car.Vehicle_Basic.Yaw;
	                double Delta_Roll =  (double) sim.vars.spaceDrivingVar__Target_Initiatial_Location_And_Rotation.Ob6 - the_driver_car.Vehicle_Basic.Roll;
	                
	                a_line_of_human_or_model_driving_results = new String[] { 
	                    Double.toString(GlobalUtilities.round(SimSystem.clock(),3)), 
	                    Double.toString(GlobalUtilities.round(Delta_X,3)), Double.toString(GlobalUtilities.round(Delta_Y,3)), Double.toString(GlobalUtilities.round(Delta_Z,3)),
	                    Double.toString(GlobalUtilities.round(Delta_Pitch,3)), Double.toString(GlobalUtilities.round(Delta_Yaw,3)), Double.toString(GlobalUtilities.round(Delta_Roll,3)),
	                    
	                    Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_X,3)),  Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_Y,3)), Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_Z,3)),
	                    Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_Pitch,3)),  Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_Yaw,3)), Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Speed_Roll,3)),
	                    
	                    Double.toString(sim.vars.deviceModule.Joystick_X), Double.toString(sim.vars.deviceModule.Joystick_Y), Double.toString(sim.vars.deviceModule.Joystick_Z),
	                    Double.toString(sim.vars.deviceModule.Joystick_Pitch), Double.toString(sim.vars.deviceModule.Joystick_Yaw), Double.toString(sim.vars.deviceModule.Joystick_Roll)
	                    
	                };  
	              }
	              
	              else{
	                a_line_of_human_or_model_driving_results = new String[] { Double.toString(GlobalUtilities.round(SimSystem.clock(),3 )), Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Distance_From_Start ,3)),  Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center ,3)), Double.toString(the_driver_car.Vehicle_Basic.Speed),   Double.toString(the_driver_car.Vehicle_Basic.Angle_Between_Road_Direction),  Double.toString(the_driver_car.Steer_Angle), Double.toString(the_driver_car.Accelerator_Pedal), Double.toString(the_driver_car.Brake_Pedal) }; 
	              }
	              sim.funs.ProgramUtilitiesFun__Output_Human_Drive_Results_Txt(  sim.funs.ProgramUtilitiesFun__StringArray_To_String_Show_Empty(a_line_of_human_or_model_driving_results) );
	              
	              
	              // experiment_driving_and_comprehension 
	              if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_driving_and_comprehension")){
	                ((LinkedList<Double>)sim.vars.programGlobalVar__Hashtable_Experiment_Specific_Data.get("Block_Results_LP")).addLast(the_driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center);
	                
	                a_line_of_human_or_model_driving_results = new String[] { Double.toString(GlobalUtilities.round(SimSystem.clock() ,3)),  Double.toString(GlobalUtilities.round(the_driver_car.Vehicle_Basic.Distance_From_Start ,3)), Double.toString(the_driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center),  Double.toString(the_driver_car.Vehicle_Basic.Speed),   Double.toString(the_driver_car.Vehicle_Basic.Angle_Between_Road_Direction),  Double.toString(the_driver_car.Steer_Angle), Double.toString(the_driver_car.Accelerator_Pedal), Double.toString(the_driver_car.Brake_Pedal) }; 
	                String a_line = sim.funs.ProgramUtilitiesFun__StringArray_To_String_Show_Empty(a_line_of_human_or_model_driving_results) + "\n";
	                String new_Driving_Raw_Data = (String)sim.vars.programGlobalVar__Hashtable_Experiment_Specific_Data.get("Driving_Raw_Data") + a_line;
	                sim.funs.ProgramUtilitiesFun__Hashtable_Add_OR_Set_Value( sim.vars.programGlobalVar__Hashtable_Experiment_Specific_Data, "Driving_Raw_Data", new_Driving_Raw_Data);
	              }
	              
	              
	            }
        	  }// end of old method, use internal 3D model
        	  
            break;
            
          case Ending:
        	  if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_opends" )){
        		  //not sure, leave it blank as for torcs      		  
                	
              } //end of model_drive_opends
        	  if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_torcs" )){
        		  //TODO        		  
                	
              } //end of model_drive_torcs
        	  else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "unity_tangtang_2015" )){
        		  //TODO        		  
                	
              } 
        	  
        	  else{ //old method, use internal 3D model
	            
	            //refresh each World3D object
	            double time_step = sim.vars.world3DTemplate.World.Refresh_Cycle;
	            //first rounds
	            Iterator<Entry> itrEntries = sim.vars.world3DTemplate.World.Objects.entrySet().iterator(); //            for(DictionaryEntry an_object :  sim.vars.world3DTemplate.World.Objects){ 
	            while(itrEntries.hasNext()){
	              Entry an_object = itrEntries.next();
	
	              if(an_object.getValue() instanceof World3D_DriverCar){
	                World3D_DriverCar driver_car = (World3D_DriverCar)an_object.getValue();
	                
	                //if automatic machine perfect driving
	                World3D_Template_Driving_Method  driving_method_pointer =  sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
	                
	                
	                
	                if(driving_method_pointer.Who_Drive.equals( "perfect_machine")){
	                  if( driving_method_pointer.Perfect_Machine_Driving_At_Speed <= 0.0 ) {
	                    System.out.println("Error! driving_method_pointer.Perfect_Machine_Driving_At_Speed <= 0.0 when Who_Drive == model");
	                    SimSystem.abort();
	                  }
	                  //assume perfectly following the center of the lane, and therefore all movement is along the road direction.
	                  double speed =  ((World3D_Template_Driving_Method)sim.vars.world3DTemplate.Method_Object).Perfect_Machine_Driving_At_Speed;
	                  double distance_moved_in_this_step = speed * time_step;
	                  
	                  //update DriverCar info
	                  driver_car.Vehicle_Basic.Distance_From_Start += distance_moved_in_this_step;
	                  Three_Tuple global_info_after_this_step = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(driver_car.Vehicle_Basic.On_Road_World3D_ID, (double)driver_car.Vehicle_Basic.Distance_From_Start,(double) driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center, 0.0F);
	                  driver_car.Vehicle_Basic.Loc_X = (double) global_info_after_this_step.Ob1;
	                  driver_car.Vehicle_Basic.Loc_Z = (double) global_info_after_this_step.Ob2;
	                  driver_car.Vehicle_Basic.Yaw = (double) global_info_after_this_step.Ob3;
	                  driver_car.Vehicle_Basic.Speed = (double) speed;
	                  if(sim.vars.animator3DModule.Show_Animator3D){
	                    
	                    System.out.println("TODO Server logics world3dcyclicrefresh, ending need Animator3D mechanism");
	//TODO           
	//                    Animator3D.MoveRotateScale(driver_car.Vehicle_Basic.Animator3D_Object_ID,  driver_car.Vehicle_Basic.Loc_X, driver_car.Vehicle_Basic.Loc_Y + driver_car.Vehicle_Basic.Camera_Height,  driver_car.Vehicle_Basic.Loc_Z,  driver_car.Vehicle_Basic.Yaw, 0.0,0.0,  sim.vars.animator3DModule.DriverCar_Shade_Scale_X ,1.0, sim.vars.animator3DModule.DriverCar_Shade_Scale_Z,  time_step);
	                  }
	                  
	                } //end of automatic perfect driving
	                
	                //sp-driv
	                
	                else if ((driving_method_pointer.Who_Drive.toLowerCase().equals( "model-6dof-speed")) || driving_method_pointer.Who_Drive.toLowerCase().equals( "human-6dof-speed")){ 
	                  
	                  System.out.println("TODO Server logics world3dcyclicrefresh, ending need Human_Drive mechanism");
	//TODO           
	                  
	//                  if (driving_method_pointer.Who_Drive.toLowerCase().equals( "human-6dof-speed")){
	//                    
	//                    if(!Human_Drive.MethodHumanDrive.equals( "SpaceDrivingKey"))System.out.println("Error! World3D_Cyclic_Refresh has when driving_method_pointer.Who_Drive == human-6DOF-speed, Human_Drive.MethodHumanDrive not equal to SpaceDrivingKey, but : " + Human_Drive.MethodHumanDrive );
	//                    
	//                    //old method
	//                    //physical movement computation in Human_Drive Plugin.
	//                    /*
	//                  driver_car.Vehicle_Basic.Loc_X += Human_Drive.Speed_X * time_step;
	//                  driver_car.Vehicle_Basic.Loc_Y += Human_Drive.Speed_Y * time_step;
	//                  driver_car.Vehicle_Basic.Loc_Z += Human_Drive.Speed_Z * time_step;
	//                  
	//                  driver_car.Vehicle_Basic.Pitch += Human_Drive.Angular_Speed_Pitch * time_step; // do not wrap into +- 180 degree, otherwise will jerk almost 360 from -179 to 179
	//                  driver_car.Vehicle_Basic.Yaw += Human_Drive.Angular_Speed_Yaw * time_step; 
	//                  driver_car.Vehicle_Basic.Roll += Human_Drive.Angular_Speed_Roll * time_step;
	//                     */
	//                    
	//                    //new method
	//                    //physical movement computation  here
	//                    //6DOF naming is the MSS way, see World3D_Vehicle_Basic object for details.
	//                    double position_acc_constant_in_meter_ss = sim.vars.spaceDrivingVar__Position_Acc_Constant ;  // m / ss
	//                    double rotation_acc_constant = sim.vars.spaceDrivingVar__Rotation_Acc_Constant ;  // deg / ss
	//                    
	//                    double position_acc_constant = position_acc_constant_in_meter_ss * 100.0 ;  // cm / ss, 
	//                    
	//                    int Stick_Press_Value_X = Human_Drive.Get_Stick_Press_Value_X(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_X = (double) (Stick_Press_Value_X * position_acc_constant);
	//                    int Stick_Press_Value_Y = Human_Drive.Get_Stick_Press_Value_Y(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Y = (double) (Stick_Press_Value_Y * position_acc_constant);
	//                    int Stick_Press_Value_Z = Human_Drive.Get_Stick_Press_Value_Z(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Z = (double) (Stick_Press_Value_Z * position_acc_constant);
	//                    
	//                    int Stick_Press_Value_Pitch = Human_Drive.Get_Stick_Press_Value_Pitch(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Pitch = (double) (Stick_Press_Value_Pitch * rotation_acc_constant);
	//                    int Stick_Press_Value_Yaw = Human_Drive.Get_Stick_Press_Value_Yaw(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Yaw = (double) (Stick_Press_Value_Yaw * rotation_acc_constant);
	//                    int Stick_Press_Value_Roll = Human_Drive.Get_Stick_Press_Value_Roll(); //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Roll = (double) (Stick_Press_Value_Roll * rotation_acc_constant);
	//                    
	//                    sim.funs.TaskTemplateFun__Update_World3D_Vehicle_3D_Dynamics_With_Constant_Accel(driver_car.Vehicle_Basic, sim.vars.world3DTemplate.World.Refresh_Cycle);
	//                    
	//                    
	//                  }
	//                  else if (driving_method_pointer.Who_Drive.toLowerCase().equals( "model-6dof-speed")){
	//                    
	//                    //6DOF naming is the MSS way, see World3D_Vehicle_Basic object for details.
	//                    double position_acc_constant_in_meter_ss = sim.vars.spaceDrivingVar__Position_Acc_Constant ;  // m / ss
	//                    double rotation_acc_constant = sim.vars.spaceDrivingVar__Rotation_Acc_Constant ;  // deg / ss
	//                    
	//                    double position_acc_constant = position_acc_constant_in_meter_ss * 100.0 ;  // cm / ss, 
	//                    
	//                    double Stick_Press_Value_X = sim.vars.deviceModule.Joystick_X; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_X = (double) (Stick_Press_Value_X * position_acc_constant);
	//                    double Stick_Press_Value_Y = sim.vars.deviceModule.Joystick_Y; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Y = (double) (Stick_Press_Value_Y * position_acc_constant);
	//                    double Stick_Press_Value_Z = sim.vars.deviceModule.Joystick_Z; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Z = (double) (Stick_Press_Value_Z * position_acc_constant);
	//                    
	//                    double Stick_Press_Value_Pitch = sim.vars.deviceModule.Joystick_Pitch; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Pitch = (double) (Stick_Press_Value_Pitch * rotation_acc_constant);
	//                    double Stick_Press_Value_Yaw = sim.vars.deviceModule.Joystick_Yaw; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Yaw = (double) (Stick_Press_Value_Yaw * rotation_acc_constant);
	//                    double Stick_Press_Value_Roll = sim.vars.deviceModule.Joystick_Roll; //can be -1, 0, or 1
	//                    driver_car.Vehicle_Basic.Local_Acc_Roll = (double) (Stick_Press_Value_Roll * rotation_acc_constant);
	//                    
	//                    sim.funs.TaskTemplateFun__Update_World3D_Vehicle_3D_Dynamics_With_Constant_Accel(driver_car.Vehicle_Basic, sim.vars.world3DTemplate.World.Refresh_Cycle);
	//                    
	//                    //System.out.println(sim.vars.deviceModule.Joystick_X);
	//                    
	//                  }
	//                  else{
	//                    System.out.println("Error! World3D_Cyclic_Refresh 6DOF-speed driving_method_pointer.Who_Drive undefined: " + driving_method_pointer.Who_Drive);
	//                    SimSystem.abort();
	//                  }
	//                  
	//                  
	//                  if(sim.vars.animator3DModule.Show_Animator3D){
	//                    //if ( driver_car.Vehicle_Basic.Camera_Height != 0.0 && Clock == time_step) System.out.println("WARNING! World3D_Cyclic_Refresh human-6DOF-speed driver_car.Vehicle_Basic.Camera_Height != 0 but: " + driver_car.Vehicle_Basic.Camera_Height + ". Camera_Height is changed to 0.");
	//                    Animator3D.MoveRotateScale(driver_car.Vehicle_Basic.Animator3D_Object_ID,  driver_car.Vehicle_Basic.Loc_X, driver_car.Vehicle_Basic.Loc_Y ,  driver_car.Vehicle_Basic.Loc_Z,  driver_car.Vehicle_Basic.Yaw, driver_car.Vehicle_Basic.Pitch, driver_car.Vehicle_Basic.Roll,  sim.vars.animator3DModule.DriverCar_Shade_Scale_X ,1.0, sim.vars.animator3DModule.DriverCar_Shade_Scale_Z,  time_step);
	//                    
	//                    // cross scope move , not accurate.
	//                    //Animator3D.MoveRotateScale( aaa_temp_ob_id,  driver_car.Vehicle_Basic.Loc_X + 0.38, driver_car.Vehicle_Basic.Loc_Y - 17.35,  driver_car.Vehicle_Basic.Loc_Z + 20.0,  0.0, 0.0,0.0,  0.1 ,0.1, 0.1,  time_step);
	//                    
	//                  }
	//                  
	                  
	                }//end of 6DOF-speed
	                
	                
	                
	                else if (driving_method_pointer.Who_Drive.equals( "model") || driving_method_pointer.Who_Drive.equals( "human") || (driving_method_pointer.Who_Drive.equals( "model-1DOF-Z-speed")) || driving_method_pointer.Who_Drive.equals( "human-1DOF-Z-speed")){ 
	                  
	                  if(driving_method_pointer.Who_Drive.equals( "human")) System.out.println("TODO Server logics world3dcyclicrefresh, ending need Human Drive mechanism");
	//TODO           
	                  
	                  //when the model is driving, here update the change on driver car's location and speed and heading after each refresh cycle.
	                  
	                  if(!driver_car.Vehicle_Basic.Frozen) {
	//TODO                     
	//                    if (driving_method_pointer.Who_Drive.equals( "human")){ //update steering and accelbrake
	//                      //NOTE: this requires related .dll file in the MicroSaintSharp directory
	//                      if (!Human_Drive.MethodHumanDrive.equals( "DrivingButton")) System.out.println("Error! World3D_Cyclic_Refresh when driving_method_pointer.Who_Drive == human, Human_Drive.MethodHumanDrive not equal DrivingButton, but: " + Human_Drive.MethodHumanDrive);
	//                      sim.funs.TaskTemplateFun__Update_DriverCar_Steer_And_Wheel_With_New_Steer( Human_Drive.Steering_Angle );
	//                      //TaskTemplateFun__Update_DriverCar_Accelbrake( Human_Drive.Accelbrake );
	//                      driver_car.Accelerator_Pedal = Human_Drive.Accelerator; // 0 - 1
	//                      driver_car.Brake_Pedal = Human_Drive.Brake; // 0 - 1
	//                    }
	                    
	                    // this should be identical to the human study's car physical model
	                    //below is a simple car physical model, assume wheel direction is the car's heading direction, no skidding, no oversteer nor understeer
	                    
	                    //first get delta_distance_moved
	                    double delta_distance_moved;
	                    double new_speed_magnitude;
	                    
	                    if(driving_method_pointer.Who_Drive.equals( "model") || driving_method_pointer.Who_Drive.equals( "human") ){
	                      double min_speed = 0.1; // for easier physical computation based on engine power.
	                      double old_speed_magnitude = driver_car.Vehicle_Basic.Speed;
	                      double old_speed_magnitude_original = driver_car.Vehicle_Basic.Speed;
	                      if ( old_speed_magnitude < min_speed ) old_speed_magnitude  =  min_speed;   // cannot backup
	                      double engine_power_output = driver_car.Accelerator_Pedal * driver_car.Engine_Power_Max;
	                      double engine_force = engine_power_output / old_speed_magnitude;
	                      double air_drag_force = driver_car.Air_Drag_Coeff * old_speed_magnitude * old_speed_magnitude;
	                      double road_friction_force = driving_method_pointer.DriverCar_Road_Surface_Friction_Coeff * driver_car.Mass * driving_method_pointer.Standard_Gravity;
	                      double brake_force = driver_car.Brake_Pedal * driver_car.Brake_Force_Max;
	                      double acceleration_value = (  engine_force -  air_drag_force -  road_friction_force - brake_force) / driver_car.Mass; //here value does not include 2D direction, but can be < 0, which mean deceleration.
	                      
	                      new_speed_magnitude = old_speed_magnitude + acceleration_value * time_step;
	                      //System.out.println("speed old: " + old_speed_magnitude + ", new_speed_magnitude: " + new_speed_magnitude + ", engine_force: " + engine_force + ", air_drag_force: " + air_drag_force + ", road_friction_force : " + road_friction_force + ", brake_force: " + brake_force);      
	                      
	                      //if (new_speed_magnitude <= 0.0) new_speed_magnitude = min_speed; // cannot backup
	                      if (new_speed_magnitude < min_speed) new_speed_magnitude = 0.0; // cannot backup
	                      
	                      
	                      //for automatically fixed speed conditions
	                      if (driving_method_pointer.Auto_Speed != 0.0) new_speed_magnitude = driving_method_pointer.Auto_Speed;
	                      
	                      
	                      //delta_distance_moved = ( old_speed_magnitude + new_speed_magnitude) / 2 * time_step; //straight or curve
	                      delta_distance_moved = ( old_speed_magnitude_original + new_speed_magnitude) / 2 * time_step; //straight or curve
	                      //System.out.println("delta_distance_moved: "+ delta_distance_moved);      
	                    }
	                    
	                    
	                    /*
	                  else if (driving_method_pointer.Who_Drive.equals( "human-1DOF-Z-speed")){
	                    
	                    if(!Human_Drive.MethodHumanDrive.equals( "SpaceDrivingKey"))System.out.println("Error! World3D_Cyclic_Refresh has when driving_method_pointer.Who_Drive == human-1DOF-Z-speed, Human_Drive.MethodHumanDrive not equal SpaceDrivingKey, but : " + Human_Drive.MethodHumanDrive );
	                    new_speed_magnitude = Human_Drive.Speed_Z;
	                    delta_distance_moved = new_speed_magnitude * time_step; 
	                  }
	                  else if (driving_method_pointer.Who_Drive.equals( "model-1DOF-Z-speed")){
	                    new_speed_magnitude = driver_car.Vehicle_Basic.Speed;
	                    delta_distance_moved = new_speed_magnitude * time_step; 
	                  }
	                     */
	                    
	                    else{
	                      System.out.println("Error! World3D_Cyclic_Refresh driving_method_pointer.Who_Drive undefined: " + driving_method_pointer.Who_Drive);
	                      SimSystem.abort();
	                      new_speed_magnitude = 0.0;
	                      delta_distance_moved = 0.0;
	                    }
	                    
	                    
	                    // don't know why, but without this the model will stop when initial speed is 0.0
	                    if(SimSystem.clock() == sim.vars.world3DTemplate.World.Refresh_Cycle && delta_distance_moved == 0.0) {
	                      delta_distance_moved = 0.000001;
	                    }
	                    //System.out.println("delta_distance_moved: " +    delta_distance_moved ); 
	                    
	                    double wheel_angle = driver_car.Wheel_Angle; //in degree
	                    
	                    //use global parameters to compute destination global parameters, then figure out local parameters
	                    double X = driver_car.Vehicle_Basic.Loc_X;
	                    double Z = driver_car.Vehicle_Basic.Loc_Z;
	                    double Yaw = driver_car.Vehicle_Basic.Yaw; // angle in degree between North (Z+ direction) and car heading
	                    double Yaw_in_radian = Yaw / 180 * Math.PI;
	                    double X_new, Z_new, Yaw_new_in_radian;
	                    
	                    if( wheel_angle == 0.0){//straight move
	                      X_new = X + delta_distance_moved * Math.sin( Yaw_in_radian );
	                      Z_new = Z + delta_distance_moved * Math.cos( Yaw_in_radian );
	                      Yaw_new_in_radian = Yaw_in_radian;
	                    }
	                    else if( Math.abs(wheel_angle) >= 90 ){
	                      System.out.println("Error! World3D_Cyclic_Refresh has Math.Abs(wheel_angle) >= 90, wheel_angle: " + wheel_angle);
	                      SimSystem.abort();
	                      X_new = 0.0;
	                      Z_new = 0.0;
	                      Yaw_new_in_radian = 0.0;
	                    }
	                    else{ // turning move
	                      double wheel_angle_in_radian = wheel_angle / 180 * Math.PI;
	                      // imagine there is a wheel in the middle of the two front wheels point A, the curve distance moved by point A is the delta_distance_moved
	                      double A_X = X + driver_car.Distance_Center_To_Front_Wheel_Line * Math.sin( Yaw_in_radian );
	                      double A_Z = Z + driver_car.Distance_Center_To_Front_Wheel_Line * Math.cos( Yaw_in_radian );
	                      double A_Yaw = Yaw;
	                      double A_Yaw_in_radian  = Yaw_in_radian ;
	                      double car_turn_radius  = driver_car.Wheel_Base_Lzz / Math.abs( Math.sin (wheel_angle_in_radian) );
	                      double total_turned_angle_magnitude_in_radian = delta_distance_moved / car_turn_radius;
	                      double direct_line_length =  Math.abs( 2.0 * car_turn_radius * Math.sin( total_turned_angle_magnitude_in_radian / 2.0) );
	                      double A_X_new = A_X + direct_line_length * Math.sin( A_Yaw_in_radian +  ( Math.signum( wheel_angle ) * total_turned_angle_magnitude_in_radian ) / 2.0     );
	                      double A_Z_new = A_Z + direct_line_length * Math.cos( A_Yaw_in_radian +  ( Math.signum( wheel_angle ) * total_turned_angle_magnitude_in_radian ) / 2.0     );
	                      double A_Yaw_new_in_radian = A_Yaw_in_radian + ( Math.signum( wheel_angle ) * total_turned_angle_magnitude_in_radian );
	                      Yaw_new_in_radian = A_Yaw_new_in_radian;
	                      X_new = A_X_new - driver_car.Distance_Center_To_Front_Wheel_Line * Math.sin( Yaw_new_in_radian );
	                      Z_new = A_Z_new - driver_car.Distance_Center_To_Front_Wheel_Line * Math.cos( Yaw_new_in_radian );
	                    }
	                    
	                    
	                    double Yaw_new_in_degree = Yaw_new_in_radian / Math.PI * 180.0;
	                    
	                    driver_car.Vehicle_Basic.Loc_X = (double) X_new;
	                    driver_car.Vehicle_Basic.Loc_Z = (double) Z_new;
	                    driver_car.Vehicle_Basic.Yaw = (double) Yaw_new_in_degree;
	                    
	                    //get local parameters from global numbers
	                    Three_Tuple local_info_after_this_step = sim.funs.ProgramUtilitiesFun__World3D_Global_Loc_And_Angle_To_Local( driver_car.Vehicle_Basic.On_Road_World3D_ID, (double)X_new,(double) Z_new  , (double)Yaw_new_in_degree, (double)driver_car.Vehicle_Basic.Distance_From_Start);
	                    
	                    if(local_info_after_this_step.Ob1 != null)driver_car.Vehicle_Basic.Distance_From_Start = (double) local_info_after_this_step.Ob1;
	                    if(local_info_after_this_step.Ob2 != null)driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center = (double) local_info_after_this_step.Ob2;
	                    if(local_info_after_this_step.Ob3 != null)driver_car.Vehicle_Basic.Angle_Between_Road_Direction = (double) local_info_after_this_step.Ob3;
	                    
	                    //update other driver_car info
	                    driver_car.Vehicle_Basic.Speed = (double) new_speed_magnitude;
	                    
	                    //System.out.println("X: " + driver_car.Vehicle_Basic.Loc_X + ", Z: " + driver_car.Vehicle_Basic.Loc_Z + ". Dist.: " + driver_car.Vehicle_Basic.Distance_From_Start + ", Lateral: " + driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center );     
	                    if(sim.vars.animator3DModule.Show_Animator3D){
	                      //Animator3D.MoveRotateScale(driver_car.Vehicle_Basic.Animator3D_Object_ID,  driver_car.Vehicle_Basic.Loc_X, driver_car.Vehicle_Basic.Loc_Y + driver_car.Vehicle_Basic.Camera_Height,  driver_car.Vehicle_Basic.Loc_Z,  driver_car.Vehicle_Basic.Yaw, 0.0,0.0,  sim.vars.animator3DModule.DriverCar_Shade_Scale_X ,1.0, sim.vars.animator3DModule.DriverCar_Shade_Scale_Z,  time_step);
	                      
	                      //Java3D use local movement .mul method
	                      double totalTurnAngle = Yaw_new_in_degree - Yaw;
	                      double halfTurnAngle = totalTurnAngle / 2.0;
	                      double moveDistance = Math.sqrt(  (X_new - X) * (X_new - X) + (Z_new - Z) * (Z_new - Z));
	                      
	                      //1. turn half angle
	                      sim.funs.AnimatorModuleFun__Animator3D_Driving_Turn_Driver_Car(-halfTurnAngle); // add - to change from MSS left hand to Java right hand.
	                      
	                      //2. move direct distance
	                      sim.funs.AnimatorModuleFun__Animator3D_Driving_Move_Driver_Car(moveDistance);  // move forward
	                      
	                      //3. turn another half angle
	                      sim.funs.AnimatorModuleFun__Animator3D_Driving_Turn_Driver_Car(-halfTurnAngle); // add - to change from MSS left hand to Java right hand.
	                                           
	                      
	                      //System.out.println("driver_car.Vehicle_Basic.Distance_From_Start: " + driver_car.Vehicle_Basic.Distance_From_Start + " X: " + driver_car.Vehicle_Basic.Loc_X + " Z: " + driver_car.Vehicle_Basic.Loc_Z);       
	                    }
	                  } //end of !frozen
	                  else{ // frozen
	                    driver_car.Vehicle_Basic.Speed = 0.0F;
	                    
	                    //experiment_driving_and_comprehension 
	                    if(sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "experiment_driving_and_comprehension") && driving_method_pointer.Auto_Speed != 0.0){
	                      //AnimatorModuleFun__Animator3D_Show_Hide_All_Comments(true);
	                      driver_car.Vehicle_Basic.Speed = driving_method_pointer.Auto_Speed;
	                    }
	                    
	                    
	                    driver_car.Steer_Angle = 0.0F;
	                    if(sim.vars.animator3DModule.Show_Animator3D){
	                      //TODO
	                      //Animator3D.MoveRotateScale(driver_car.Vehicle_Basic.Animator3D_Object_ID,  driver_car.Vehicle_Basic.Loc_X, driver_car.Vehicle_Basic.Loc_Y + driver_car.Vehicle_Basic.Camera_Height,  driver_car.Vehicle_Basic.Loc_Z,  driver_car.Vehicle_Basic.Yaw, 0.0,0.0,  sim.vars.animator3DModule.DriverCar_Shade_Scale_X ,1.0, sim.vars.animator3DModule.DriverCar_Shade_Scale_Z,  time_step);
	                    }
	                  }
	                  
	                }//end of "human" or "model" driving the car
	                
	                
	                
	                
	                
	                //              else if (driving_method_pointer.Who_Drive.equals( "human")){
	                //System.out.println("ToDo human driving");
	                
	                //              }
	                
	                else {
	                  System.out.println("Error! World3D_Cyclic_Refresh has driving_method_pointer.Who_Drive undefined value: " + driving_method_pointer.Who_Drive);
	                  SimSystem.abort();
	                  
	                }
	                
	                //System.out.println("World3D_Cyclic_Refresh: " + driver_car.Vehicle_Basic.Distance_From_Start + " " + ((World3D_Road)sim.vars.world3DTemplate.World.Objects[driver_car.Vehicle_Basic.On_Road_World3D_ID]).Distance_Total);
	                
	                
	                
	                //update Animator3D comments
	                if(sim.vars.animator3DModule.Show_Animator3D){
	//                  System.out.println("TODO Server logics world3dcyclicrefresh, ending need Show_Animator3D mechanism");
	      
	//                  if(!driver_car.Vehicle_Basic.Frozen) Animator3D.SetCommentText(4, "Who drive: " + driving_method_pointer.Who_Drive);
	//                  else Animator3D.SetCommentText(4, "Who drive: " + driving_method_pointer.Who_Drive + " Frozen.");
	//                  
	//                  Animator3D.SetCommentText(1, "Distance (m): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Distance_From_Start)));
	//                  Animator3D.SetCommentText(2, "Lateral D (m): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center)));
	//                  Animator3D.SetCommentText(3, "Speed (m/s): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Speed)));
	//                  Animator3D.SetCommentText(5, "X: " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Loc_X)));
	//                  Animator3D.SetCommentText(6, "Y: " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Loc_Y)));
	//                  Animator3D.SetCommentText(7, "Z: " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Loc_Z)));
	//                  Animator3D.SetCommentText(8, "Pitch (degree): " +Double.toString( GlobalUtilities.round(driver_car.Vehicle_Basic.Pitch)));
	//                  Animator3D.SetCommentText(9, "Yaw (degree): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Yaw)));
	//                  Animator3D.SetCommentText(10, "Roll (degree): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Roll)));
	//                  
	//                  Animator3D.SetCommentText(21, "Accel: " + Double.toString(GlobalUtilities.round(driver_car.Accelerator_Pedal)));
	//                  Animator3D.SetCommentText(22, "Brake: " + Double.toString(GlobalUtilities.round(driver_car.Brake_Pedal)));
	//                  Animator3D.SetCommentText(23, "Steer (degree): " + Double.toString(GlobalUtilities.round(driver_car.Steer_Angle)));
	//                  Animator3D.SetCommentText(24, "Accelbrake: " + Double.toString(GlobalUtilities.round(driving_method_pointer.Accelbrake)));
	                  
	                  System.out.println("Distance (m): " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Distance_From_Start, 2)));
	                  System.out.println("Z: " + Double.toString(GlobalUtilities.round(driver_car.Vehicle_Basic.Loc_Z, 1)));
	
	                  
	                  
	                }
	                
	                if(driver_car.Vehicle_Basic.Distance_From_Start >=  ((World3D_Road)sim.vars.world3DTemplate.World.Objects.get(driver_car.Vehicle_Basic.On_Road_World3D_ID)).Distance_Total){
	                  System.out.println("Driving reaches the end of the road.");
	                  SimSystem.abort();
	                }
	                
	                sim.vars.varCopy__DriverCar_Speed = driver_car.Vehicle_Basic.Speed; // pass to .dll plugin
	                sim.vars.varCopy__DriverCar_Pitch = driver_car.Vehicle_Basic.Pitch;
	                sim.vars.varCopy__DriverCar_Yaw = driver_car.Vehicle_Basic.Yaw;
	                sim.vars.varCopy__DriverCar_Roll = driver_car.Vehicle_Basic.Roll;
	              } //end of World3D_DriverCar
	              
	              
	              
	              
	              else if(an_object.getValue() instanceof World3D_OtherCar){
	                World3D_OtherCar other_car = (World3D_OtherCar)an_object.getValue();
	                World3D_Template_Driving_Method  driving_method_pointer =  sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
	                
	                if(!other_car.Vehicle_Basic.Frozen) {
	                  //assume perfectly following the center of the lane, and therefore all movement is along the road direction.
	                  double speed =  other_car.Speed ;
	                  double distance_moved_in_this_step = speed * time_step;
	                  
	                  //update Car info
	                  other_car.Vehicle_Basic.Distance_From_Start += distance_moved_in_this_step;
	                  Three_Tuple global_info_after_this_step = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global( other_car.Vehicle_Basic.On_Road_World3D_ID, other_car.Vehicle_Basic.Distance_From_Start, other_car.Vehicle_Basic.Distance_Lateral_To_Center_Lane_Center, 0.0F);
	                  other_car.Vehicle_Basic.Loc_X = (double) global_info_after_this_step.Ob1;
	                  other_car.Vehicle_Basic.Loc_Z = (double) global_info_after_this_step.Ob2;
	                  other_car.Vehicle_Basic.Yaw = (double) global_info_after_this_step.Ob3;
	                } 
	                else{
	                  
	                }
	                if(sim.vars.animator3DModule.Show_Animator3D){
	                  System.out.println("TODO Server logics world3dcyclicrefresh, ending need Show_Animator3D mechanism World3D_OtherCar");
	//TODO                    
	//                  Animator3D.MoveRotateScale(other_car.Vehicle_Basic.Animator3D_Object_ID,  other_car.Vehicle_Basic.Loc_X, other_car.Vehicle_Basic.Loc_Y,  other_car.Vehicle_Basic.Loc_Z,  other_car.Vehicle_Basic.Yaw, 0.0,0.0,  sim.vars.animator3DModule.OtherCar_Scale ,sim.vars.animator3DModule.OtherCar_Scale, sim.vars.animator3DModule.OtherCar_Scale,  time_step);
	//                  if(driving_method_pointer.Visual_Attention_Location_World3D_ID.equals(other_car.World3D_ID)){          
	//                    Animator3D.SetColor(other_car.Vehicle_Basic.Animator3D_Object_ID , Color.Red) ;
	//                  }
	//                  else {
	//                    Animator3D.SetColor(other_car.Vehicle_Basic.Animator3D_Object_ID , Color.Blue)  ;
	//                  }
	                }
	                
	                //restart if out of road distance range
	                if(other_car.Vehicle_Basic.Distance_From_Start >=  ((World3D_Road)sim.vars.world3DTemplate.World.Objects.get(other_car.Vehicle_Basic.On_Road_World3D_ID)).Distance_Total) other_car.Vehicle_Basic.Distance_From_Start = other_car.Start_Distance;
	                
	              } //end of World3D_OtherCar
	              
	              
	              
	              else if (an_object.getValue() instanceof World3D_Road){
	                
	                // if use, will slow down the simulation.
	                /*
	              if( Clock % 5 == 0 ){
	                World3D_DriverCar the_driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
	                sim.funs.TaskTemplateFun__Update_World3D_Road_Fragments_Animator3D_Show_Or_Hide(the_driver_car.Vehicle_Basic.Loc_X , the_driver_car.Vehicle_Basic.Loc_Z, sim.vars.programGlobalVar__World3D_Show_Road_Fragment_Range, (World3D_Road) an_object.getValue());
	              }
	                 */
	                
	              } //end of World3D_Road
	              else if (an_object.getValue() instanceof World3D_Line_Segment){
	                //nothing
	                //System.out.println("World3D_Cyclic_Refresh an_object.getValue() is World3D_Line_Segment");  
	              } //end of World3D_Line_Segment
	              else if (an_object.getValue() instanceof World3D_Point){
	                //wait for DriverCar to update
	              } //end of World3D_Point
	              else{
	                System.out.println("Error! World3D_Cyclic_Refresh has undefined world3d object type");  
	                SimSystem.abort();
	              }
	              
	            } //end of first round
	            
	            //second round
	            Iterator<Entry> itrEntries1 = sim.vars.world3DTemplate.World.Objects.entrySet().iterator(); //            for(DictionaryEntry an_object :  sim.vars.world3DTemplate.World.Objects){ 
	            while(itrEntries1.hasNext()){
	              Entry an_object = itrEntries1.next();
	              
	             
	              if (an_object.getValue() instanceof World3D_Point){
	                World3D_Point the_point = (World3D_Point)an_object.getValue() ;
	                
	                if (the_point.Type.equals( "customized-local-point")){
	                  
	                  //currently , no moving of customized-local-point
	                  World3D_Template_Driving_Method  driving_method_pointer =  sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
	                  
	                  if(sim.vars.animator3DModule.Show_Animator3D){
	                    System.out.println("TODO Server logics world3dcyclicrefresh, ending need Show_Animator3D mechanism World3D_Point");
	//TODO       
	//                    if(driving_method_pointer.Visual_Attention_Location_World3D_ID.equals(the_point.World3D_ID)){          
	//                      Animator3D.SetColor(the_point.Animator3D_Object_ID , Color.Red) ;
	//                    }
	//                    else {
	//                      Animator3D.SetColor(the_point.Animator3D_Object_ID , the_point.Color) ;
	//                    }
	                  }
	                  
	                }
	                else if(the_point.Type.length() >= 9 && the_point.Type.substring(0,9).equals( "far-point")){
	                  World3D_DriverCar driver_car = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
	                  World3D_Template_Driving_Method  driving_method_pointer =  sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();
	                  the_point.Distance_From_Start = driver_car.Vehicle_Basic.Distance_From_Start + driving_method_pointer.Far_Point_Time_Ahead * driver_car.Vehicle_Basic.Speed ;
	                  Three_Tuple global_para = sim.funs.ProgramUtilitiesFun__World3D_Local_Loc_And_Angle_To_Global(the_point.On_Road_World3D_ID, (double)the_point.Distance_From_Start, (double)the_point.Distance_Lateral_To_Center_Lane_Center, 0.0F);
	                  the_point.Loc_X = (double) global_para.Ob1;
	                  the_point.Loc_Z = (double) global_para.Ob2;
	                  
	                  if(sim.vars.animator3DModule.Show_Animator3D){
	                    System.out.println("TODO Server logics world3dcyclicrefresh, ending need Show_Animator3D mechanism World3D_Point");
	//TODO                           
	//                    Animator3D.MoveRotateScale(the_point.Animator3D_Object_ID,  the_point.Loc_X, -1 * 1.5 * sim.vars.animator3DModule.Far_Point_Scale,  the_point.Loc_Z,  0.0, 0.0,0.0,  sim.vars.animator3DModule.Far_Point_Scale ,sim.vars.animator3DModule.Far_Point_Scale, sim.vars.animator3DModule.Far_Point_Scale,  time_step);
	//                    if(driving_method_pointer.Visual_Attention_Location_World3D_ID.equals(the_point.World3D_ID)){          
	//                      Animator3D.Show(the_point.Animator3D_Object_ID) ;
	//                    }
	//                    else {
	//                      Animator3D.Hide(the_point.Animator3D_Object_ID);
	//                    }
	                    
	                  }
	                }
	                else{
	                  System.out.println("Error! World3D_Cyclic_Refresh has undefined Point type: " + the_point.Type);
	                  SimSystem.abort();
	                }
	              } //end of World3D_Point
	            }
	            
	            if( sim.vars.world3DTemplate.Method_Object != null ){
	              if (sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) {
	                sim.funs.VisionModuleFun__Update_World3D_Driving_Method_Visible_Objects();
	              }
	            }
	            
	            /*
	          // for Space Drive 1D speed control demo
	          World3D_DriverCar driver_car2 = sim.funs.TaskTemplateFun__Get_World3D_DriverCar_Object();
	          if ( driver_car2.Vehicle_Basic.Distance_From_Start >= 950 && driver_car2.Vehicle_Basic.Distance_From_Start <= 1500 && driver_car2.Vehicle_Basic.Speed == 0.0){
	            System.out.println("Space Drive 1D speed control demo Task Complete!");  
	            SimSystem.abort();
	          }
	             */
        	  } //end of old method, use internal 3D model
            break;
            
          case Timing:
        	  if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_opends" )){
        		  World3D_Template_Driving_Method the_method_3 = null;
	              if(sim.vars.world3DTemplate.Method_Object != null && sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) the_method_3 = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();        		  
        		  
		          the_method_3.sendControlToOpenDS();
		          the_method_3.receivePerceptEarlyFromOpenDS();
        		  
	              double cycleTime =  the_method_3.getOpenDSPercept().OpenDSClock - SimSystem.clock();
	              
	              return cycleTime; 
	              
              } //end of model_drive_opends
        	  if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "model_drive_torcs" )){
        		  World3D_Template_Driving_Method the_method_3 = null;
	              if(sim.vars.world3DTemplate.Method_Object != null && sim.vars.world3DTemplate.Method_Object instanceof World3D_Template_Driving_Method) the_method_3 = sim.funs.TaskTemplateFun__Get_World3D_Driving_Method_Object();        		  
        		  
	              the_method_3.sendControlToTORCS();
		          the_method_3.receivePerceptEarlyFromTORCS();
        		  
	              double cycleTime =  the_method_3.getTorcsPercept().TORCSClock - SimSystem.clock();
	              
	              
	              //temp for easier debug
	              //cycleTime = 0.02;
	              
	              return cycleTime; 
	              
              } //end of model_drive_torcs
        	  else if (sim.vars.programGlobalVar__Use_Predefined_Model_Setup.equals( "unity_tangtang_2015" )){
        		  
        		  UnityJavaUdp unityUDPtool = null;
	              if(sim.vars.world3DTemplate.Method_Object != null && sim.vars.world3DTemplate.Method_Object instanceof UnityJavaUdp) unityUDPtool = (UnityJavaUdp)sim.vars.world3DTemplate.Method_Object;        		  
	              else {
	            	  JOptionPane.showMessageDialog(null, "Error: " + ServerName , "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
	            	  return 0;
	              }
	            	
	              //JOptionPane.showMessageDialog(null, "before receive: " + ServerName , "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
	              unityUDPtool.Receive(); //just receive the data from unity. when visual module look at the target, it will use this received distance data
	              
	              //JOptionPane.showMessageDialog(null, "before send: " + ServerName , "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
	              unityUDPtool.Send();  //just send the most recent decided status. the status will be updated once the production rule is fired.
	              
	              //JOptionPane.showMessageDialog(null, "after send: " + ServerName , "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE);
        		  
	              double cycleTime =  unityUDPtool.sdreceive.c1 - SimSystem.clock();
	              
	              
	              //temp for easier debug
	              //cycleTime = 0.02;
	              
	              return cycleTime; 
	              
                }
        	  else{ //old method, use internal 3D model
        		  return sim.vars.world3DTemplate.World.Refresh_Cycle; //in second
        	  }
        }
        break;
        
      default:
        JOptionPane.showMessageDialog(null, "Enums has undefined serverName: " + ServerName , "ServerLogics.java" , JOptionPane.INFORMATION_MESSAGE); //QN-Java
        switch (ServiceStage){
          
          case Release:
            
            return true;
            
          case Beginning:
            break;
            
          case Ending:
            
            break;
            
          case Timing:
            return 0.0;
            
          default:
            System.out.println("Error! ServerLogics has undefined ServiceStage: " + ServiceStage + " for ServerName: " + ServerName);
            break;  
        }
        break;
        
    }
    return null; // if break from any switch of ServerNames with no return value.
  }

private void ChunkFun__Print_Chunk() {
	// TODO Auto-generated method stub
	
}
}
