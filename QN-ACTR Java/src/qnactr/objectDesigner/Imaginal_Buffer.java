package qnactr.objectDesigner;

public class Imaginal_Buffer {
	
	public boolean Empty;
	public double Imaginal_Activation;
	public Chunk Imaginal_Buffer_Chunk = new Chunk();
	
	public Imaginal_Buffer(){
		
		Empty = true;
		Imaginal_Activation = 0.0F;
  }

}