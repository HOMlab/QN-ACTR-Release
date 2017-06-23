package qnactr.objectDesigner;

public class Display_Item_Visual_Text {

	
	public boolean As_Paragraph;
	public String Correct_Response = "";
	public String Display_Item_Color;
	public double Display_Item_Delay;
	public double Display_Item_Duration;
	public int Display_Item_Screen_Location_X;
	public int Display_Item_Screen_Location_Y;
	public String Feedback = "";
	public String Feedback_Item_Color;
	public double Feedback_Item_Delay;
	public double Feedback_Item_Duration;
	public int Feedback_Item_Screen_Location_X;
	public int Feedback_Item_Screen_Location_Y;
	public boolean Hide;
	public String Item_ID = "";
	public String Item_Name = "";
	public String Key_Press_Event = "";
	public int Paragraph_Char_Per_Line;
	public int Paragraph_Line_Num;
	public String Visual_Text = "";
	
	public Display_Item_Visual_Text(){
		
		As_Paragraph = false;
		Display_Item_Color = "black";
		Display_Item_Delay = 0.0F;
		Display_Item_Duration = -1.0F;
		Display_Item_Screen_Location_X = 0;
		Display_Item_Screen_Location_Y = 0;
		Feedback_Item_Color = "black";
		Feedback_Item_Delay = 0.0F;
		Feedback_Item_Duration = 0.0F;
		Feedback_Item_Screen_Location_X = 0;
		Feedback_Item_Screen_Location_Y = 0;
		Hide = false;
		Paragraph_Char_Per_Line = 0;
		Paragraph_Line_Num = 0;
		
		
	}
	
}


