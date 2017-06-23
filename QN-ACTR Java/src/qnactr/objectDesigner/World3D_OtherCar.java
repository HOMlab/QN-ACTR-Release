package qnactr.objectDesigner;

public class World3D_OtherCar {

	public double Max_Speed;
	public String Mode = "";
	public double Speed;
	public double Start_Distance;
	public int Start_Lane_Num;
	public String Start_Road_Name = "";
	public String Start_Road_World3D_ID = "";
	public String Type = "";
	public World3D_Vehicle_Basic Vehicle_Basic;
	public String World3D_ID = "";
	
	public World3D_OtherCar(){
		Max_Speed=50.0F;
		Speed=10.0F;
		Start_Distance=0.0F;
		Start_Lane_Num=-1;
		Vehicle_Basic=new World3D_Vehicle_Basic();
	}
}