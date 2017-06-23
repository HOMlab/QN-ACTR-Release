package jmt.engine.jwat.input;

import jmt.engine.jwat.MatrixOsservazioni;

public class EventFinishLoad implements EventStatus {

	private MatrixOsservazioni m;
	private int valToRead;
	private int valReaded;

	public EventFinishLoad(MatrixOsservazioni m, int valToRead, int valReaded) {
		this.m = m;
		this.valReaded = valReaded;
		this.valToRead = valToRead;
	}

	public MatrixOsservazioni getSession() {
		return m;
	}

	public int valToRead() {
		return valToRead;
	}

	public int valReaded() {
		return valReaded;
	}

	public int getType() {
		return EventStatus.DONE_EVENT;
	}

}
