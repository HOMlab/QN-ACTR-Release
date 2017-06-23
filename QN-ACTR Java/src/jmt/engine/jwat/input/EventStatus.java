package jmt.engine.jwat.input;

public interface EventStatus {

	public final static int ABORT_EVENT = 0;
	public final static int DONE_EVENT = 1;

	public int getType();
}
