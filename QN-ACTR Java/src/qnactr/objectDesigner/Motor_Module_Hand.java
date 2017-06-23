package qnactr.objectDesigner;
import java.util.*;
public class Motor_Module_Hand {

	public Hashtable<String, Two_Tuple> Finger_Location_Offsets = new Hashtable<String, Two_Tuple>();
	public Two_Tuple Location = new Two_Tuple();
	public String Object_Type_In_Hand = "";
	
	public Two_Tuple Latest_Move_To_Location = new Two_Tuple();
	
	public Motor_Module_Hand(){
		
		Latest_Move_To_Location.Ob1 = "";
		Latest_Move_To_Location.Ob2 = "";
		
	}
}
