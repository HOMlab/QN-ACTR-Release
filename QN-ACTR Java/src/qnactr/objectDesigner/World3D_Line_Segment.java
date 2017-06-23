package qnactr.objectDesigner;

public class World3D_Line_Segment {

	public String Animator3D_Object_ID = "";
	public int Color;//Type: System.Drawing.Color.Black
	public String On_Road_World3D_ID = "";
	public double P1_Distance_From_Start;
	public double P1_Distance_Lateral_To_Center_Lane_Center;
	public double P1_Loc_X;
	public double P1_Loc_Y;
	public double P1_Loc_Z;
	public double P2_Distance_From_Start;
	public double P2_Distance_Lateral_To_Center_Lane_Center;
	public double P2_Loc_X;
	public double P2_Loc_Y;
	public double P2_Loc_Z;
	public String Type = "";
	public String World3D_ID = "";
	
	
	public World3D_Line_Segment(){
		Color = 0; //Black
		P1_Distance_From_Start=0.0F;
		P1_Distance_Lateral_To_Center_Lane_Center=0.0F;
		P1_Loc_X=0.0F;
		P1_Loc_Y=0.0F;
		P1_Loc_Z=0.0F;
		P2_Distance_From_Start=0.0F;
		P2_Distance_Lateral_To_Center_Lane_Center=0.0F;
		P2_Loc_X=0.0F;
		P2_Loc_Y=0.0F;
		P2_Loc_Z=0.0F;
	}
	
	public World3D_Line_Segment Clone () {
	  try
    {
      return (World3D_Line_Segment) this.clone();
    } catch (CloneNotSupportedException e)
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
	}
}
