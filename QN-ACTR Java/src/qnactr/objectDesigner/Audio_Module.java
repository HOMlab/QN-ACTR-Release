package qnactr.objectDesigner;
import java.util.*;
public class Audio_Module {

	public Hashtable<String, Double> Audicon_Decay_Time_Tracking_Table = new Hashtable<String, Double>();
	public LinkedList<String> Buffer_Stuffing_Just_Selected_Attended_Nil_Aural_Location_Names= new LinkedList<String>();
	public double Digit_Detect_Delay;
	public double Digit_Duration;
	public double Digit_Recode_Delay;
	public boolean Listening_To_OnesOwn_Speech;
	public double Sound_Decay_Time;
	public boolean State_Error;
	public boolean State_Free;
	public boolean State_Preparation_Free;
	public double Tone_Detect_Delay;
	public double Tone_Recode_Delay;
	
	public Audio_Module(){
		
		Digit_Detect_Delay = 0.3;
		Digit_Duration = 0.6;
		Digit_Recode_Delay = 0.5;
		Listening_To_OnesOwn_Speech = true;
		Sound_Decay_Time = 3.0;
		State_Error = false;
		State_Free = true;
		State_Preparation_Free = true;
		Tone_Detect_Delay = 0.05;
		Tone_Recode_Delay = 0.285;
	}
	
}
