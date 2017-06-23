package jmt.engine.jwat.input;

public interface ProgressShow {

	public void eventUpdate(int value, String txt);

	public boolean isCanceled();

	public void closeView();

	public void initShow(int maxValue);

	public int getStep();
}
