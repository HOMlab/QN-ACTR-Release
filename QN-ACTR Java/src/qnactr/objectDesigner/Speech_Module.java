package qnactr.objectDesigner;

import java.util.*;

public class Speech_Module {
	public Hashtable Articulation_Time_Table = new Hashtable(); //store the articulation time for each string like "hello". can be used both in speech and audio for generating and processing a word
	public int Char_Per_Syllable;
	public  boolean Execution_Free;
	public  Chunk Last_Command =new Chunk();
	public  double Pause_Between_Words_In_Sentence;
	public  boolean Preparation_Free;
	public  boolean Processor_Free;
	public  double Speech_Initiation_Time;
	public  boolean State_Free;
	public  double Subvocalize_Detect_Delay;
	public double Syllable_Rate;
	
	public Speech_Module(){
		Char_Per_Syllable=3;
		Execution_Free=true;
		Pause_Between_Words_In_Sentence=0.0F;
		Preparation_Free=true;
		Processor_Free=true;
		Speech_Initiation_Time=0.050F;
		State_Free=true;
		Subvocalize_Detect_Delay=0.3F;
		Syllable_Rate=0.15F;
	}
}
