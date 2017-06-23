package qnactr.objectDesigner;


public class Retrieval_Buffer {
	public boolean Empty;
	public double Retrieval_Activation;
	public Chunk Retrieval_Buffer_Chunk =new Chunk();
	
	public Retrieval_Buffer(){
		Empty=true;
		Retrieval_Activation=0.0F;
	}
}
