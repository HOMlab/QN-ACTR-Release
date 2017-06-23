package jmt.engine.jwat.input;

import java.awt.Component;

import javax.swing.ProgressMonitor;

public class ProgressMonitorShow implements ProgressShow {
	private ProgressMonitor pm;
	private int step;

	public ProgressMonitorShow(Component container, String msg, int step) {
		super();
		this.step = step;
		pm = new ProgressMonitor(container, msg, "<HTML>-<p>-</HTML>", 0, 0);
	}

	public void eventUpdate(int value, String txt) {
		pm.setProgress(value);
		pm.setNote(txt);
	}

	public boolean isCanceled() {
		return pm.isCanceled();
	}

	public void closeView() {
		pm.close();
	}

	public void initShow(int maxValue) {
		pm.setMaximum(maxValue);
		pm.setProgress(0);
		pm.setMillisToDecideToPopup(0);
		pm.setMillisToPopup(0);
	}

	public int getStep() {
		return step;
	}
}
