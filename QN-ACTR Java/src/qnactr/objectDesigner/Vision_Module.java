package qnactr.objectDesigner;

import java.util.*;

public class Vision_Module {

	public LinkedList<String> Buffer_Stuffing_Just_Selected_Attended_Nil_Visual_Location_Names= new LinkedList<String>();
	public String Last_Attended_Screen_X = "";
	public String Last_Attended_Screen_Y = "";
	public String Last_Attended_Visicon_Name = "";
	public String Last_Attended_Visual_Location_Name = "";
	public  boolean Moving_Attention;
	public boolean Reencoding_Enabled;
	public  boolean Reencoding_Makes_Vision_Busy;
	public boolean State_Error;
	public  boolean State_Free;
	public  boolean State_Preparation_Free;
	public int Text_Char_Group_Length;
	public  boolean Trigger_Procedural_Conflict_Resolution_After_Visual_Location_Buffer_Stuffing;
	public  double Visual_Attention_Latency;
	public LinkedList<Integer> Visual_Finst_ID_Track=new LinkedList<Integer>();
	public double Visual_Finst_Span;
	public double Visual_Movement_Tolerance;
	public int Visual_Num_Finsts;
	public double Visual_Onset_Span;
	
	public Vision_Module(){
		Moving_Attention=false;
		Reencoding_Enabled=true;
		Reencoding_Makes_Vision_Busy=true;
		State_Error=false;
		State_Free=true;
		State_Preparation_Free=true;
		Text_Char_Group_Length=11;
		Trigger_Procedural_Conflict_Resolution_After_Visual_Location_Buffer_Stuffing=true;
		Visual_Attention_Latency=0.085F;
		Visual_Finst_Span=3.0F;
		Visual_Movement_Tolerance=0.5F;
		Visual_Num_Finsts=4;
		Visual_Onset_Span=0.5F;
	}
}
