package qnactr.objectDesigner;


public class Visual_Location_Buffer {

	public boolean Empty;
	public  boolean Requested;
	public  boolean State_Error;
	public  boolean State_Free;
	public  boolean Unrequested;
	public double Visual_Location_Activation;
	public Chunk Visual_Location_Buffer_Chunk=new Chunk();
	
	public Visual_Location_Buffer(){
		Empty=true;
		Requested=false;
		State_Error=false;
		State_Free=true;
		Unrequested=false;
		Visual_Location_Activation=0.0F;
	}
}
