package qnactr.objectDesigner;

/**
 * This is used to store and pass model driving perception values from OpenDS to QN-Java
 * 
 * @author Yelly
 *
 */
public class OpenDS_Percept {
	
	public double nearPointAngleDegree;
	public double farPointAngleDegree;
	public double farPointDistanceMeter;
	public double speed; // m/s
	public double OpenDSClock;
	
	public OpenDS_Percept(){
		nearPointAngleDegree = 0.0;
		farPointAngleDegree = 0.0;
		farPointDistanceMeter = 0.0;
		speed = 0.0;
		OpenDSClock = 0.0;
	}
	
	public void copyTo (OpenDS_Percept target){
		target.nearPointAngleDegree = this.nearPointAngleDegree;
		target.farPointAngleDegree = this.farPointAngleDegree;
		target.farPointDistanceMeter = this.farPointDistanceMeter;
		target.speed = this.speed;
		target.OpenDSClock = this.OpenDSClock;
	}
	
}
