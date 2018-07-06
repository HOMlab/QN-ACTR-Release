package qnactr.objectDesigner;

import java.util.Hashtable;

/**
 * This is used to store and pass model driving perception values from TORCS to QN-Java
 * 
 * @author Shi
 *
 */
public class Torcs_Percept {
	
	//public double nearPointAngleDegree;
	//public double farPointAngleDegree;\
	public Hashtable<Integer, Double> nearPointAngleDegree = new Hashtable<Integer, Double> ();
	public Hashtable<Integer, Double> farPointAngleDegree = new Hashtable<Integer, Double> ();
	public double farPointDistanceMeter;
	public double speed; // m/s
	public double TORCSClock;
	
	public double leftObjDist;
	public double leftMirrorObjDist;
	public double rightObjDist;
	public double rightMirrorObjDist;	
	
	
	public Torcs_Percept(){
		nearPointAngleDegree.put(-1, 0.0);
		nearPointAngleDegree.put(1, 0.0);
		nearPointAngleDegree.put(+1, 0.0);
		
		farPointAngleDegree.put(-1, 0.0);
		farPointAngleDegree.put(1, 0.0);
		farPointAngleDegree.put(+1, 0.0);
				
		
		farPointDistanceMeter = 0.0;
		speed = 0.0;
		TORCSClock = 0.0;
	}
	

	/*
	public void copyTo (Torcs_Percept target){
		target.nearPointAngleDegree = this.nearPointAngleDegree;
		target.farPointAngleDegree = this.farPointAngleDegree;
		target.farPointDistanceMeter = this.farPointDistanceMeter;
		target.speed = this.speed;
		target.TORCSClock = this.TORCSClock;
	}
	*/
	
}
