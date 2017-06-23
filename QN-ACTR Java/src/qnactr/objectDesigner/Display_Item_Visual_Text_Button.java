package qnactr.objectDesigner;

public class Display_Item_Visual_Text_Button {

	public String Button_Click_Event = "";
	public String Correct_Response = "";
	public String Display_Button_Color;
	public String Display_Item_Color;
	public double Display_Item_Delay;
	public double Display_Item_Duration;
	public int Display_Item_Height;
	public int Display_Item_Screen_Location_X;
	public int Display_Item_Screen_Location_Y;
	public int Display_Item_Width;
	public String Feedback = "";
	public String Feedback_Item_Color;
	public double Feedback_Item_Delay;
	public double Feedback_Item_Duration;
	public int Feedback_Item_Screen_Location_X;
	public int Feedback_Item_Screen_Location_Y;
	public boolean Hide;
	public String Item_ID = "";
	public String Item_Name = "";
	public String Visual_Text = "";
	
	public Display_Item_Visual_Text_Button(){
		
		Display_Button_Color= "gray";
		Display_Item_Color = "black";
		Display_Item_Delay = 0.0F;
		Display_Item_Duration = -1.0F;
		Display_Item_Height = 20;
		Display_Item_Screen_Location_X = 0;
		Display_Item_Screen_Location_Y = 0;
		Display_Item_Width = 20;
		Feedback_Item_Color = "black";
		Feedback_Item_Delay = 0.0F;
		Feedback_Item_Duration = 0.0F;
		Feedback_Item_Screen_Location_X = 0;
		Feedback_Item_Screen_Location_Y = 0;
		Hide = false;
		
		
		
	}
	
}


