package qnactr.objectDesigner;


public class World3D_Point implements Cloneable{

	public String Animator3D_Object_ID = "";
	public int Color; //Type:System.Drawing.Color
	public double Distance_From_Start;
	public double Distance_Lateral_To_Center_Lane_Center;
	public double Loc_X;
	public double Loc_Y;
	public double Loc_Z;
	public String Name = "";
	public int On_Road_Lane;
	public String On_Road_World3D_ID = "";
	public double Size_X;
	public double Size_Y;
	public double Size_Z;
	public String Type = "";
	public String World3D_ID = "";
	
	public World3D_Point(){
		Color=0;//System.Drawing.Color.Black
		Distance_From_Start=0.0F;
		Distance_Lateral_To_Center_Lane_Center=0.0F;
		Loc_X=0.0F;
		Loc_Y=0.0F;
		Loc_Z=0.0F;
		On_Road_Lane=0;
		Size_X=1.0F;
		Size_Y=1.0F;
		Size_Z=1.0F;
	}
	

	@Override
	public Object clone() {
	  try
    {
      return super.clone();
    } catch (CloneNotSupportedException e)
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
      return null;
    }
	}
	
}
