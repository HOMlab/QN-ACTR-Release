package qnactr.objectDesigner;

public class Display_Item_Visual_Line {

	
	public String Correct_Response = "";
	public String Display_Item_Color;
	public double Display_Item_Delay;
	public double Display_Item_Duration;
	public int End_Point_Screen_Location_X;
	public int End_Point_Screen_Location_Y;
	public String Feedback = "";
	public String Feedback_Item_Color;
	public double Feedback_Item_Delay;
	public double Feedback_Item_Duration;
	public int Feedback_Item_Screen_Location_X;
	public int Feedback_Item_Screen_Location_Y;
	public boolean Hide;
	public String Item_ID = "";
	public String Item_Name = "";
	public double Length;
	public int Start_Point_Screen_Location_X;
	public int Start_Point_Screen_Location_Y;
	
	public Display_Item_Visual_Line(){
		
		Display_Item_Color = "black";
		Display_Item_Delay = 0.0F;
		Display_Item_Duration = -1.0F;
		End_Point_Screen_Location_X = 0;
		End_Point_Screen_Location_Y = 0;
		Feedback_Item_Color = "black";
		Feedback_Item_Delay = 0.0F;
		Feedback_Item_Duration = 0.0F;
		Feedback_Item_Screen_Location_X = 0;
		Feedback_Item_Screen_Location_Y = 0;
		Hide = false;
		Length = 0.0F;
		Start_Point_Screen_Location_X = 0;
		Start_Point_Screen_Location_Y = 0;
		
	}
	
}
