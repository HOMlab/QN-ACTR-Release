package qnactr.objectDesigner;

import java.util.*;

public class World3D {

	public int Object_Num;
	public Hashtable Objects=new Hashtable(); // <string Object ID, an World3D object>
	public double Refresh_Cycle;

	
	public World3D(){
		Object_Num=0;
		Refresh_Cycle = 0.010;
	}
}
