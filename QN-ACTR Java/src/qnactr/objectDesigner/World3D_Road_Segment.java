package qnactr.objectDesigner;

public class World3D_Road_Segment {

	public double Distance_End;
	public double Distance_Start;
	public double End_Loc_X;
	public double End_Loc_Z;
	public double Length;
	public double Start_Heading_Angle;
	public double Start_Loc_X;
	public double Start_Loc_Z;
	public double Turn_Angle;
	public String Type;
	
	public World3D_Road_Segment(){
		Distance_End=0.0F;
		Distance_Start=0.0F;
		End_Loc_X=0.0F;
		End_Loc_Z=0.0F;
		Length=1000.0F;
		Start_Heading_Angle=0.0F;
		Start_Loc_X=0.0F;
		Start_Loc_Z=0.0F;
		Turn_Angle=0.0F;
		Type="straight";
	}
	
}
