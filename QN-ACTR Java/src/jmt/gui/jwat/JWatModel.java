package jmt.gui.jwat;

import jmt.engine.jwat.MatrixOsservazioni;

public interface JWatModel {
	public void resetModel();

	public MatrixOsservazioni getMatrix();

	public void setMatrix(MatrixOsservazioni matrix);
}
