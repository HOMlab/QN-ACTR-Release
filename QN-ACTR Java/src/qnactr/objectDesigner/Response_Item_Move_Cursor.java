package qnactr.objectDesigner;



public class Response_Item_Move_Cursor {
	public double Clock_Time;
	public double Duration;
	public int End_X;
	public int End_Y;
	public double Reaction_Time;
	public boolean Response_Correctness;
	public int Start_X;
	public int Start_Y;
	
	public Response_Item_Move_Cursor(){
		Clock_Time=0.0F;
		Duration=0.0F;
		End_X=0;
		End_Y=0;
		Reaction_Time=0.0F;
		Response_Correctness=false;
		Start_X=0;
		Start_Y=0;
	}
}
