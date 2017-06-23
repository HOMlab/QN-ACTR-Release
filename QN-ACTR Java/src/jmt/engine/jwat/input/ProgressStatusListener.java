package jmt.engine.jwat.input;

import jmt.engine.jwat.MatrixOsservazioni;

public interface ProgressStatusListener {

	public void abortEvent();

	public void finishedEvent(MatrixOsservazioni m, int MaxToRead, int Readed);
}
