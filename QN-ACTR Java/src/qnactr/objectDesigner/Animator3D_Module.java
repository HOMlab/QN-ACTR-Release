/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;

public class Animator3D_Module {

	public String DriverCar_Model_ID;
	public double DriverCar_Shade_Scale_X;
	public double DriverCar_Shade_Scale_Z;
	public double Far_Point_Scale;
	public String OtherCar_Model_ID;
	public double OtherCar_Scale;
	public String Point_Model_ID;
	public boolean Show_Animator3D;
	
	public Animator3D_Module(){
		DriverCar_Model_ID = "Plane10By10";
		DriverCar_Shade_Scale_X = 0.18;
		DriverCar_Shade_Scale_Z = 0.48;
		Far_Point_Scale = 0.2;
		OtherCar_Model_ID = "Car";;
		OtherCar_Scale = 0.24;
		Point_Model_ID = "Sphere3Foot";
		Show_Animator3D = false;
	}
	
	
}
