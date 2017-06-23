package qnactr.objectDesigner;


public class World3D_DriverCar {
	public double Accelerator_Pedal;
	public double Air_Drag_Coeff;
	public double Brake_Force_Max;
	public double Brake_Pedal;
	public double Distance_Center_To_Front_Wheel_Line;
	public double Distance_Center_To_Side_Wheel_Line;
	public double Engine_Power_Max;
	public double Mass;
	public double Start_Distance;
	public int Start_Lane_Num;
	public String Start_Road_Name = "";
	public String Start_Road_World3D_ID = "";
	public double Steer_Angle;
	public double Steering_Ratio;
	public World3D_Vehicle_Basic Vehicle_Basic;
	public double Wheel_Angle;
	public double Wheel_Base_Lzz;
	
	
	public World3D_DriverCar(){
		Accelerator_Pedal=0.0F;
		Air_Drag_Coeff=0.25F;
		Brake_Force_Max=8000.0F;
		Brake_Pedal=0.0F;
		Distance_Center_To_Front_Wheel_Line=1.719F;
		Distance_Center_To_Side_Wheel_Line=0.946F;
		Engine_Power_Max=106000F;
		Mass=1175.0F;
		Start_Distance=0.0F;
		Start_Lane_Num=0;
		Steer_Angle=0.0F;
		Steering_Ratio=20F;
		Vehicle_Basic=new World3D_Vehicle_Basic();
		Wheel_Angle=0.0F;
		Wheel_Base_Lzz=2.618F;
	}
}
