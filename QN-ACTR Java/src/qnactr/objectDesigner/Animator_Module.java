/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;
import java.util.*;

public class Animator_Module {

	public Hashtable<String, String> Audicon_Name_Comment_ID_Table = new Hashtable<String, String>();
	public boolean Play_Sound_Through_Speaker;
	public boolean Show_Animator;
	public boolean Show_Network_Details_Visualization;
	public boolean Show_Visual_Attention_Focus;
	public Hashtable<String, String> Visicon_Name_Comment_ID_Table = new Hashtable<String, String>();
	public Hashtable<String, String> Visicon_Name_Image_ID_Table = new Hashtable<String, String>();
	public int Visual_Attention_Offset_Screen_X;
	public int Visual_Attention_Offset_Screen_Y;
	public boolean Vocalize_Speech;
	
	public Animator_Module(){
		
		Play_Sound_Through_Speaker = true;
		Show_Animator= true;
		Show_Network_Details_Visualization= true;
		Show_Visual_Attention_Focus = false;
		Visual_Attention_Offset_Screen_X = -10;
		Visual_Attention_Offset_Screen_Y = -10;
		Vocalize_Speech = true;
		
	}
	
}
