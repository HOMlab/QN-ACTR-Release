

package qnactr.objectDesigner;
import java.util.*;
public class Central_Parameters_Module {
  
  public Hashtable Chunk_Types_Description = new Hashtable();
  public Hashtable<String, String> Chunk_Types_Include = new Hashtable<String, String>();
  public Hashtable Chunk_Types_Numbers = new Hashtable();
  public Hashtable Chunks = new Hashtable();
  public boolean er;
  public boolean esc;
  public LinkedList<Chunk> Experiment_Grouped_Result_Identifier_List=new LinkedList<Chunk>();
  public Hashtable Experiment_Grouped_Result_Table = new Hashtable();
  public LinkedList<LinkedList<Object>> Experiment_Trial_Display_Item_List=new LinkedList<LinkedList<Object>>();
  public LinkedList<Hashtable> Experiment_Trial_Parameter_List = new LinkedList<Hashtable>();
  public int Experiment_Trial_Parameter_List_Line_Number;
  public LinkedList<LinkedList<Object>> Experiment_Trial_Response_Item_List=new LinkedList<LinkedList<Object>>();
  public LinkedList<Hashtable> Experiment_Trial_Result_List = new LinkedList<Hashtable>();
  public int Eye_Movement_Results_Line_Number;
  public int Foot_Results_Line_Number;
  public int Human_Drive_Results_Line_Number;
  public String ol;
  public double Pi;
  public boolean Randomize_Seed_For_Display_Sequence;
  public String randomize_time;
  public int Response_Results_Line_Number;
  public LinkedList<String> Special_Chunk_Types_For_Programming= new LinkedList<String>();
  public boolean Stop_When_No_In_Model_Event_Pending;
  public int Trace_Line_Number;
  public boolean Use_Procedural_Resources;
  
  public Central_Parameters_Module(){
    er = false;
    esc = false;
    Experiment_Trial_Parameter_List_Line_Number = 1;
    Eye_Movement_Results_Line_Number = 1;
    Foot_Results_Line_Number = 1;
    Human_Drive_Results_Line_Number = 1;
    ol = "t";
    Pi = Math.PI;
    Randomize_Seed_For_Display_Sequence = true;
    randomize_time = "nil";
    Response_Results_Line_Number = 1;
    Stop_When_No_In_Model_Event_Pending = true;
    Trace_Line_Number = 1;
    Use_Procedural_Resources = false;
    }
  
}
