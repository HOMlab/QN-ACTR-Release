package qnactr.objectDesigner;
import java.util.*;
public class Device_Module {
	
	public double Joystick_Pitch;
	public double Joystick_Roll;
	public double Joystick_X;
	public double Joystick_Y;
	public double Joystick_Yaw;
	public double Joystick_Z;
	public double Key_Closure_Time;
	public Hashtable Key_To_Location_Hashtable = new Hashtable();
	public int Mouse_Cursor_Initial_Screen_X;
	public int Mouse_Cursor_Initial_Screen_Y;
	public int Mouse_Cursor_Screen_X;
	public int Mouse_Cursor_Screen_Y;
	public int Pixcels_Per_Inch;
	public Hashtable Press_Key_To_Motor_Command_Hashtable = new Hashtable();
	public double Viewing_Distance;
	public boolean isTextboxInputMode = false;
	public String focusedTextboxInput = "";
	
	public Device_Module(){
		
		Joystick_Pitch = 0.0;
		Joystick_Roll = 0.0;
		Joystick_X = 0.0;
		Joystick_Y = 0.0;
		Joystick_Yaw = 0.0;
		Joystick_Z = 0.0;
		Key_Closure_Time = 0.010;
		 Mouse_Cursor_Initial_Screen_X = 0;
		 Mouse_Cursor_Initial_Screen_Y = 0;
		Mouse_Cursor_Screen_X = 0;
		Mouse_Cursor_Screen_Y = 0;
		Pixcels_Per_Inch = 72;
		Viewing_Distance = 15.0; //inch
		
	}
	
	public boolean enterTextboxInputMode ( Display_Item_Visual_Text_Button textbox ) {
		if(isTextboxInputMode) {
			//assume that there could be only one focused textbox in the input mode. If input mode is true, return error. 
			System.out.println("Error! Device_Module enterTextboxInputMode cannot enter when isTextboxInputMode is already true.");
			return false;
		}
		
		isTextboxInputMode = true;
		focusedTextboxInput = "";
		textbox.Visual_Text = focusedTextboxInput;
		
		return true;
	}
	
	
	public boolean textboxTyping (Display_Item_Visual_Text_Button textbox , String key) {
		if(!isTextboxInputMode) {
			System.out.println("Error! Device_Module textboxTyping cannot typing when isTextboxInputMode is false.");
			return false;
		}
		
		if (key.equals("return")) {
			//call function to endTextboxInputMode
			this.endTextboxInputMode (textbox);
			return true;
		}
		else {
			focusedTextboxInput = focusedTextboxInput + key;
			textbox.Visual_Text = focusedTextboxInput;
			return true;
		}
		
	}
	
	public boolean endTextboxInputMode(Display_Item_Visual_Text_Button textbox) {
		if(!isTextboxInputMode) {
			System.out.println("Error! Device_Module endTextboxInputMode cannot end nothing, when isTextboxInputMode is false.");
			return false;
		}
		
		isTextboxInputMode = false;
		//focusedTextboxInput kept as is, in case other functions may need to use it.
		return true;
		
	}
	
}
