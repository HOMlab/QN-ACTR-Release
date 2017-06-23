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
}
