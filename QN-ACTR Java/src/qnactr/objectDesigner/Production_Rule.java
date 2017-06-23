package qnactr.objectDesigner;
import java.util.*;
public class Production_Rule {
  
  public LinkedList<Production_Rule_Condition_Action_Item> Action= new LinkedList<Production_Rule_Condition_Action_Item>();
  public double Action_Time;
  public String Compilation_Father = "";
  public String Compilation_Note_For_New_Rule = "";
  public LinkedList<String> Compilation_Sons= new LinkedList<String>();
  public LinkedList<Production_Rule_Condition_Action_Item> Condition= new LinkedList<Production_Rule_Condition_Action_Item>();
  public String Condition_Part_Goal_X_Reference = "";
  public int Num_Aural_Action;
  public int Num_Aural_Location_Action;
  public int Num_Goal_Action;
  public int Num_High_Level_Requests;
  public int Num_Imaginal_Action;
  public int Num_Low_Level_Requests;
  public int Num_Manual_Action;
  public int Num_Retrieval_Action;
  public int Num_Visual_Action;
  public int Num_Visual_Location_Action;
  public int Num_Vocal_Action;
  public String Rule_Name = "";
  public Hashtable Variable_Binding = new Hashtable();
  
  public Production_Rule(){
    Action_Time=0.050F;
    Num_Aural_Action=0;
    Num_Aural_Location_Action=0;
    Num_Goal_Action=0;
    Num_High_Level_Requests=0;
    Num_Imaginal_Action=0;
    Num_Low_Level_Requests=0;
    Num_Manual_Action=0;
    Num_Retrieval_Action=0;
    Num_Visual_Action=0;
    Num_Visual_Location_Action=0;
    Num_Vocal_Action=0;
  }
  
}
