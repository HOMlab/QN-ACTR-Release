package qnactr.objectDesigner;

public class CriticalElement {
	String type; // "vehicle" or "sign"
	String content; // color for car, content for sign
	boolean front_visibility; // whether this element is within drivers front visible area
	boolean back_visibility; // whether this element is visible in back(center) mirror
	boolean leftBack_visibility; // whether this element is visible in left-back mirror
	boolean rightBack_visibility; // whether this element is visible in right-back mirror
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	public String getContent() {
		return content;
	}
	public void setContent(String content) {
		this.content = content;
	}
	public boolean isFront_visibility() {
		return front_visibility;
	}
	public void setFront_visibility(boolean front_visibility) {
		this.front_visibility = front_visibility;
	}
	public boolean isBack_visibility() {
		return back_visibility;
	}
	public void setBack_visibility(boolean back_visibility) {
		this.back_visibility = back_visibility;
	}
	public boolean isLeftBack_visibility() {
		return leftBack_visibility;
	}
	public void setLeftBack_visibility(boolean leftBack_visibility) {
		this.leftBack_visibility = leftBack_visibility;
	}
	public boolean isRightBack_visibility() {
		return rightBack_visibility;
	}
	public void setRightBack_visibility(boolean rightBack_visibility) {
		this.rightBack_visibility = rightBack_visibility;
	}
	public CriticalElement(String type, String content, boolean front_visibility, boolean back_visibility,
			boolean leftBack_visibility, boolean rightBack_visibility) {
		this.type = type;
		this.content = content;
		this.front_visibility = front_visibility;
		this.back_visibility = back_visibility;
		this.leftBack_visibility = leftBack_visibility;
		this.rightBack_visibility = rightBack_visibility;
	}
	@Override
	public String toString() {
		return "CriticalElement [type=" + type + ", content=" + content + ", front_visibility=" + front_visibility
				+ ", back_visibility=" + back_visibility + ", leftBack_visibility=" + leftBack_visibility
				+ ", rightBack_visibility=" + rightBack_visibility + "]";
	}
	

}
