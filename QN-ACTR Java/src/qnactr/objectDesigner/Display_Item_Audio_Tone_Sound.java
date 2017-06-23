package qnactr.objectDesigner;

public class Display_Item_Audio_Tone_Sound {
	
	
	public String Channel;
	public String Correct_Response = "";
	public double Display_Item_Delay;
	public double Display_Item_Duration;
	public String Feedback = "";
	public double Feedback_Item_Duration;
	public int Frequency;
	public String Item_ID = "";

	
	public Display_Item_Audio_Tone_Sound(){
		
		Channel = "left_and_right  ";
		Display_Item_Delay = 0.0;
		Display_Item_Duration = -1.0F;
		Feedback_Item_Duration = 0.0F;
		Frequency = 0;
				
	}
}