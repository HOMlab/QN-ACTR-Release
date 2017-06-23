package qnactr.objectDesigner;

import java.util.*;

public class Production_Compilation_Module {
	public  Hashtable<String,String> Buffer_Test_Symbol_List=new Hashtable<String,String>();
	public  Hashtable<String,String> Compilation_Check_Lookup_Table=new Hashtable<String,String>();
	public  boolean epl;
	public  String[][] Goal_Compilation_Check_Table=new String[24][24];
	public String Last_Fired_Rule_Name = "";
	public double Last_Rule_Fired_Time;
	public  String[][] Motor_Compilation_Check_Table=new String[24][24];
	public int Number_Of_New_Rules;
	public boolean pct;
	public  String[][] Perceptual_Compilation_Check_Table=new String[24][24];
	public  String[][] Retrieval_Compilation_Check_Table=new String[24][24];
	public  double tt;
	
	public Production_Compilation_Module(){
		epl=false;
		Last_Rule_Fired_Time=0.0F;
		Number_Of_New_Rules=0;
		pct=false;
		tt = 2.0;
	}
}
