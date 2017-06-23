package qnactr.objectDesigner;
import java.util.*;

public class Chunk {
	public double Activation;
	public String base_level;
	public String Chunk_Name = "";
	public String Chunk_Type = "";
	public double Creation_Time;
	public String DM_Name_Origin  = "";
	public double Last_Marked_T_Time;
	public double Last_Presentation_Time;
	public long Number_Of_Presentations = 0;
	public double permanent_noise;
	public LinkedList<Double> Presentation_Time_References= new LinkedList<Double>();
	public Hashtable<String, String> Slot = new Hashtable<String, String>();
	public LinkedList<String> Slot_Names_In_Order= new LinkedList<String>();
	
	public Chunk Motor_Computation_Chunk = null; // store result from MotorModuleFun__Motor_Request_Chunk_To_Motor_Computation_Chunk, to avoid repeated computation.
	
	public Chunk(){
		Activation= 0.0;
		base_level = "nil";
		Creation_Time = 0.0;
		Last_Marked_T_Time = 0.0;
		Last_Presentation_Time = 0.0;
		permanent_noise = 0.0;
	}
}
