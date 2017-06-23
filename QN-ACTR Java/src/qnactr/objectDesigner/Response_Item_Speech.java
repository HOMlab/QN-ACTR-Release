package qnactr.objectDesigner;


public class Response_Item_Speech {
	public double Clock_Time;
	public double Reaction_Time;
	public boolean Response_Correctness;
	public String Speech = "";
	
	public Response_Item_Speech(){
		Clock_Time=0.0F;
		Reaction_Time=0.0F;
		Response_Correctness=false;
	}
}
