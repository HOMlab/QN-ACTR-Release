/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;

import java.util.*;

public class Motor_Module {

	public boolean Cursor_Noise;
	public double Default_Target_Width;
	public Hashtable<String, Double> Digram_Frequency_Table = new Hashtable<String, Double>();
	public boolean Enable_Motor_Subnetwork_Decay;
	public boolean Enable_Typing_Key_Error_Due_To_Distance_Distribution;
	public boolean Enable_Typing_Key_Omission_Due_To_Little_Force;
	public boolean Execution_Free;
	public int Execution_Resource_Left_Hand;
	public int Execution_Resource_Right_Hand;
	public String Incremental_Mouse_Moves;
	public String Initiated_But_Output_Not_Finished_Request = "";
	public boolean Is_Typing_Letters;
	public Chunk Last_Command = new Chunk(); 
	public String Last_Command_Chunk_Type = "";
	public Chunk Last_Prep= new Chunk(); 
	public Motor_Module_Hand Left_Hand = new Motor_Module_Hand();
	public double Min_Fitts_Time;
	public double Motor_Burst_Time;
	public double Motor_Feature_Prep_Time;
	public double Motor_Initiation_Time;
	public boolean Motor_Output_Sequence_Required;
	public double Motor_Subnetwork_Decay_Half_Life;
	public double Mouse_Fitts_Coeff;
	public double Peck_Fitts_Coeff;
	public boolean Preparation_Free;
	public boolean Processor_Free;
	public Motor_Module_Hand Right_Hand = new Motor_Module_Hand();
	public boolean State_Free;
	public String Typing_Data_Last_Pressed_Key = "";
	public int Typing_Learning_N_Total;
	
	
	public Motor_Module(){
		
		Cursor_Noise = false;
		Default_Target_Width = 1.0;
		Enable_Motor_Subnetwork_Decay = true;
		Enable_Typing_Key_Error_Due_To_Distance_Distribution = false;
		Enable_Typing_Key_Omission_Due_To_Little_Force = false;
		Execution_Free = true;
		Execution_Resource_Left_Hand = 1;
		Execution_Resource_Right_Hand = 1;
		Incremental_Mouse_Moves = "nil";
		Is_Typing_Letters = false;
		Min_Fitts_Time= 0.1; // second
		Motor_Burst_Time = 0.05;
		Motor_Feature_Prep_Time = 0.05;
		Motor_Initiation_Time = 0.05;
		Motor_Output_Sequence_Required = false;
		Motor_Subnetwork_Decay_Half_Life = 1.0;
		Mouse_Fitts_Coeff = 0.1;
		Peck_Fitts_Coeff = 0.075;
		Preparation_Free = true;
		Processor_Free = true;
		State_Free= true;
		Typing_Learning_N_Total = 15000000;
		
	}
	
}
