package qnactr.objectDesigner;


public class Temporal_Module {
	public double Current_Tick_Length;
	public boolean State_Busy;
	public boolean State_Error;
	public boolean State_Free;
	public double Time_Master_Start_Increment;
	public double Time_Mult;
	public double Time_Noise;
	public double Time_Start_Increment;
	
	public Temporal_Module(){
		Current_Tick_Length = 0.011;
		State_Busy=false;
		State_Error=false;
		State_Free=true;
		Time_Master_Start_Increment = 0.011;
		Time_Mult = 1.1;
		Time_Noise = 0.015;
		Time_Start_Increment = -1;
	}
}
