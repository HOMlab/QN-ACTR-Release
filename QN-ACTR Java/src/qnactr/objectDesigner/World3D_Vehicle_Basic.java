package qnactr.objectDesigner;


public class World3D_Vehicle_Basic {
	
	public double Angle_Between_Road_Direction;
	public String Animator3D_Object_ID;
	public double Camera_Height;
	public double Distance_From_Start;
	public double Distance_Lateral_To_Center_Lane_Center;
	public boolean Frozen;
	public int Lane_Num;
	public double Loc_X;
	public double Loc_Y;
	public double Loc_Z;
	public double Local_Acc_Pitch;
	public double Local_Acc_Roll;
	public double Local_Acc_X;
	public double Local_Acc_Y;
	public double Local_Acc_Yaw;
	public double Local_Acc_Z;
	public String On_Road_World3D_ID;
	public double Pitch;
	public double Roll;
	public double Speed;  // m/s
	public double Speed_Pitch;
	public double Speed_Roll;
	public double Speed_X;
	public double Speed_Y;
	public double Speed_Yaw;
	public double Speed_Z;
	public int Turn_Signal;
	public double Yaw;
	
	public World3D_Vehicle_Basic(){
		Angle_Between_Road_Direction=0.0F;
		Camera_Height=1.0F;
		Distance_From_Start=0.0F;
		Distance_Lateral_To_Center_Lane_Center=0.0F;
		Frozen=false;
		Lane_Num=0;
		Loc_X=0.0F;
		Loc_Y=0.0F;
		Loc_Z=0.0F;
		Local_Acc_Pitch=0.0F;
		Local_Acc_Roll=0.0F;
		Local_Acc_X=0.0F;
		Local_Acc_Y=0.0F;
		Local_Acc_Yaw=0.0F;
		Local_Acc_Z=0.0F;
		Pitch=0.0F;
		Roll=0.0F;
		Speed=0.0F;
		Speed_Pitch=0.0F;
		Speed_Roll=0.0F;
		Speed_X=0.0F;
		Speed_Y=0.0F;
		Speed_Yaw=0.0F;
		Speed_Z=0.0F;
		Turn_Signal=0;
		Yaw=0.0F;
	}
}
