/**
 * 2013 QN-Java project file
 * 
 */

package qnactr.objectDesigner;
import java.util.*;
public class Declarative_Module {

	public boolean act;
	public String ans;
	public double blc;
	public String bll;
	public double Declarative_Finst_Span;
	public int Declarative_Num_Finsts;
	public LinkedList<String> Declarative_Retrieved_Finst_Name_List= new LinkedList<String>();
	public Hashtable Declarative_Retrieved_Finst_Time_Hashtable = new Hashtable();
	public LinkedList<Chunk> DM_Chunk=new LinkedList<Chunk>();
	public Hashtable DM_Chunk_Similarity_Table = new Hashtable();
	public double le;
	public double lf;
	public String mas;
	public double md;
	public boolean Merge_Aural_Buffer_Chunk;
	public boolean Merge_Aural_Location_Buffer_Chunk;
	public boolean Merge_Goal_Buffer_1_Chunk;
	public boolean Merge_Goal_Buffer_2_Chunk;
	public boolean Merge_Visual_Buffer_Chunk;
	public boolean Merge_Visual_Location_Buffer_Chunk;
	public String mp;
	public double ms;
	public int Number_of_Chunks;
	public Hashtable Obsolete_Chunk_Name_Number = new Hashtable();
	public double rt;
	public boolean State_Error;
	public boolean State_Free;
	
	public Declarative_Module(){
		
		act = false;
		ans = "nil";
		blc = 0.0;
		bll = "nil";
		Declarative_Finst_Span = 3.0;
	 	Declarative_Num_Finsts = 4;
	 	le = 1.0;
		lf = 1.0;
		mas = "nil";
		md = -1.0;
		Merge_Aural_Buffer_Chunk = true;
		Merge_Aural_Location_Buffer_Chunk = true;
		Merge_Goal_Buffer_1_Chunk = true;
		Merge_Goal_Buffer_2_Chunk = true;
		Merge_Visual_Buffer_Chunk = true;
		Merge_Visual_Location_Buffer_Chunk = true;
		mp = "nil";
		ms = 0.0;
		Number_of_Chunks = 0;
		rt = 0.0;
		State_Error = false;
		State_Free = true;
	}
	
}
