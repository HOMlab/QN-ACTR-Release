/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;


public class Printing_Module {
	public boolean Clear_Eye_Movement_Results_Txt_Before_Each_Simulation;
	public boolean Clear_Foot_Results_Txt_Before_Each_Simulation;
	public boolean Clear_Human_Drive_Results_Txt_Before_Each_Simulation;
	public boolean Clear_Output_Trace_Txt_Before_Each_Simulation;
	public boolean Clear_Response_Results_Txt_Before_Each_Simulation;
	public boolean Clear_Mental_Workload_Results_Txt_Before_Each_Simulation;
	public boolean Eye_Movement_Results;
	public boolean Foot_Results;
	public boolean Human_Drive_Results;
	public boolean Output_Act_R_Noise_Random_Pick_Trace;
	public boolean Output_Trace;
	public boolean Output_Trace_Append;
	public boolean Output_Window_Trace;
	public boolean Popout_Message;
	public boolean Response_Results;
	public boolean Show_Display_And_Feedback_In_Output_Trace_Txt;
	public boolean Show_Multiple_Goal_Trace_In_Output_Trace_Txt;
	public String v;
	
	public Printing_Module(){
		Clear_Eye_Movement_Results_Txt_Before_Each_Simulation=true;
		Clear_Foot_Results_Txt_Before_Each_Simulation=true;
		Clear_Human_Drive_Results_Txt_Before_Each_Simulation=true;
		Clear_Output_Trace_Txt_Before_Each_Simulation=true;
		Clear_Response_Results_Txt_Before_Each_Simulation=true;
		Clear_Mental_Workload_Results_Txt_Before_Each_Simulation = true;
		
		Eye_Movement_Results=true;
		Foot_Results=true;
		Human_Drive_Results=true;
		Output_Act_R_Noise_Random_Pick_Trace=false;
		Output_Trace=true;
		Output_Trace_Append=false;
		Output_Window_Trace=false;
		Popout_Message=true;
		Response_Results=true;
		Show_Display_And_Feedback_In_Output_Trace_Txt=false;
		Show_Multiple_Goal_Trace_In_Output_Trace_Txt=false;
		v="t";
	}
	
}
