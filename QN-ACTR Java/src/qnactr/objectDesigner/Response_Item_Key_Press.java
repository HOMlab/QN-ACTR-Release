package qnactr.objectDesigner;


public class Response_Item_Key_Press {
	public double Clock_Time;
	public String Key = "";
	public int Mouse_Cursor_X;
	public int Mouse_Cursor_Y;
	public String Note = "";
	public double Reaction_Time;
	public boolean Response_Correctness;
	
	public Response_Item_Key_Press(){
		Clock_Time=0.0F;
		Mouse_Cursor_X=0;
		Mouse_Cursor_Y=0;
		Reaction_Time=0.0F;
		Response_Correctness=false;
	}
}
