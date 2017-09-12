package qnactr.objectDesigner;

public class CriticalElement {
	String id; // unique identifier
	public String type; // "vehicle" or "sign"
	public String content; // color for car, content for sign
	boolean front_visibility; // whether this element is within drivers front visible area
	boolean left_visibility; // whether this element is visible when driver turns his head to left
	boolean right_visibility; // whether this element is visible when driver turns his head to right
	boolean insideMirror_visibility; // whether this element is visible in inside mirror
	boolean leftMirror_visibility; // whether this element is visible in left mirror
	boolean rightMirror_visibility; // whether this element is visible in right mirror
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
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
	public boolean isLeft_visibility() {
		return left_visibility;
	}
	public void setLeft_visibility(boolean left_visibility) {
		this.left_visibility = left_visibility;
	}
	public boolean isRight_visibility() {
		return right_visibility;
	}
	public void setRight_visibility(boolean right_visibility) {
		this.right_visibility = right_visibility;
	}
	public boolean isInsideMirror_visibility() {
		return insideMirror_visibility;
	}
	public void setInsideMirror_visibility(boolean insideMirror_visibility) {
		this.insideMirror_visibility = insideMirror_visibility;
	}
	public boolean isLeftMirror_visibility() {
		return leftMirror_visibility;
	}
	public void setLeftMirror_visibility(boolean leftMirror_visibility) {
		this.leftMirror_visibility = leftMirror_visibility;
	}
	public boolean isRightMirror_visibility() {
		return rightMirror_visibility;
	}
	public void setRightMirror_visibility(boolean rightMirror_visibility) {
		this.rightMirror_visibility = rightMirror_visibility;
	}
	public CriticalElement(String id, String type, String content, boolean front_visibility, boolean left_visibility,
			boolean right_visibility, boolean insideMirror_visibility, boolean leftMirror_visibility,
			boolean rightMirror_visibility) {
		this.id = id;
		this.type = type;
		this.content = content;
		this.front_visibility = front_visibility;
		this.left_visibility = left_visibility;
		this.right_visibility = right_visibility;
		this.insideMirror_visibility = insideMirror_visibility;
		this.leftMirror_visibility = leftMirror_visibility;
		this.rightMirror_visibility = rightMirror_visibility;
	}
	@Override
	public String toString() {
		return "CriticalElement [id=" + id + ", type=" + type + ", content=" + content + ", front_visibility="
				+ front_visibility + ", left_visibility=" + left_visibility + ", right_visibility=" + right_visibility
				+ ", insideMirror_visibility=" + insideMirror_visibility + ", leftMirror_visibility="
				+ leftMirror_visibility + ", rightMirror_visibility=" + rightMirror_visibility + "]";
	}

}
