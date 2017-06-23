/**
 * 
 */
package jmt.engine.jwat;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import javax.swing.SwingUtilities;

import jmt.engine.jwat.input.EventStatus;
import jmt.engine.jwat.input.ProgressShow;
import jmt.gui.jwat.SwingWorker;

/**
 * @author Maevar
 *
 */
public abstract class TimeConsumingWorker extends SwingWorker {

	private ProgressShow viewer;
	private ArrayList<ProgressStatusListener> statusListener = null; //<ProgressStatusListener>

	/**
	 * 
	 */
	public TimeConsumingWorker(ProgressShow prg) {
		super();
		viewer = prg;
		statusListener = new ArrayList<ProgressStatusListener>();
	}

	public int getStep() {
		return viewer.getStep();
	}

	public void addStatusListener(ProgressStatusListener listener) {
		statusListener.add(listener);
	}

	public void fireEventStatus(EventStatus e) {
		for (int i = 0; i < statusListener.size(); i++) {
			statusListener.get(i).statusEvent(e);
		}
	}

	public void updateInfos(final int value, final String txt, boolean waitShow) {
		Runnable r = new Runnable() {
			public void run() {
				viewer.eventUpdate(value, txt);
			}
		};
		if (waitShow) {
			try {
				SwingUtilities.invokeAndWait(r);
			} catch (InterruptedException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
		} else {
			SwingUtilities.invokeLater(r);
		}
	}

	public boolean isCanceled() {
		return viewer.isCanceled();
	}

	public void initShow(final int maxValue) throws InterruptedException, InvocationTargetException {
		Runnable r = new Runnable() {
			public void run() {
				viewer.initShow(maxValue);
			}
		};

		SwingUtilities.invokeAndWait(r);
	}

	public void closeView() {
		Runnable r = new Runnable() {
			public void run() {
				viewer.closeView();
			}
		};

		SwingUtilities.invokeLater(r);

	}

	/* (non-Javadoc)
	 * @see jmt.jwat.Utility.SwingWorker#construct()
	 */
	@Override
	public abstract Object construct();

	/* (non-Javadoc)
	 * @see jmt.jwat.Utility.SwingWorker#finished()
	 */
	@Override
	public abstract void finished();
}
