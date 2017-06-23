package qnactr.objectDesigner;

import java.util.*;

public class Task_Template {
	public boolean Auto_Compute_Default_Reaction_Time;
	public boolean Auto_Compute_Default_Response_Correctness;
	public boolean Clear_Retrieval_Buffer_And_Reset_Declarative_Module_State;
	public Hashtable Dynamic_Items=new Hashtable();
	public String Method = "";
	public boolean Obsolete_ReInitialize_Goal_Focus_Before_Each_Trial;
	public String Obsolete_Response = "";
	public String Output_Initialized_Experiment_Trial_Parameter_List_To_Experiment_Trial_Parameter_List_Txt = "";
	public boolean ReInitialize_Goal_1_Before_Each_Trial;
	public boolean ReInitialize_Intentional_Module_Before_Each_Trial;
	public boolean Reset_All_Modules_Before_Each_Day;
	public boolean Reset_All_Modules_Before_Each_Trial;
	public boolean Reset_Audio_Module_Before_Each_Trial;
	public boolean Reset_Imaginary_Module_Before_Each_Trial;
	public boolean Reset_Vision_Module_Before_Each_Trial;
	public boolean Response_Terminates_Display;
	public boolean Response_Terminates_Feedback;
	public String Task_1_Current_Stage;
	public int Task_1_Current_Trial_Number;
	public LinkedList<Object> Trial_Generating_Methods=new LinkedList<Object>();
	
	public Task_Template(){
		Auto_Compute_Default_Reaction_Time=false;
		Auto_Compute_Default_Response_Correctness=false;
		Clear_Retrieval_Buffer_And_Reset_Declarative_Module_State=false;
		Obsolete_ReInitialize_Goal_Focus_Before_Each_Trial=false;
		ReInitialize_Goal_1_Before_Each_Trial=false;
		ReInitialize_Intentional_Module_Before_Each_Trial=false;
		Reset_All_Modules_Before_Each_Day=false;
		Reset_All_Modules_Before_Each_Trial=false;
		Reset_Audio_Module_Before_Each_Trial=false;
		Reset_Imaginary_Module_Before_Each_Trial=false;
		Reset_Vision_Module_Before_Each_Trial=false;
		Response_Terminates_Display=true;
		Response_Terminates_Feedback=true;
		Task_1_Current_Stage="Not_Started";
		Task_1_Current_Trial_Number=0;
	}
}
