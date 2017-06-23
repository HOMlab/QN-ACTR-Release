package qnactr.objectDesigner;


public class Temporal_Buffer {
	public boolean Empty;
	public double Temporal_Activation;
	public Chunk Temporal_Buffer_Chunk=new Chunk();
	
	public Temporal_Buffer(){
		Empty=true;
		Temporal_Activation=0.0F;
	}
}
