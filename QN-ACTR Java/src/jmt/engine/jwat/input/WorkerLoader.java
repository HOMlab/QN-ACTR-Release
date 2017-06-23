/**
 * 
 */
package jmt.engine.jwat.input;

import java.lang.reflect.InvocationTargetException;

import javax.swing.SwingUtilities;

import jmt.gui.jwat.SwingWorker;

/**
 * @author Maevar
 *
 */
public abstract class WorkerLoader extends SwingWorker {

	private ProgressShow viewer;

	/**
	 * 
	 */
	public WorkerLoader(ProgressShow prg) {
		super();
		viewer = prg;
	}

	protected void updateInfos(final int value, final String txt, boolean waitShow) {
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

	protected boolean isCanceled() {
		return viewer.isCanceled();
	}

	protected void initShow(final int maxValue) throws InterruptedException, InvocationTargetException {
		Runnable r = new Runnable() {
			public void run() {
				viewer.initShow(maxValue);
			}
		};

		SwingUtilities.invokeAndWait(r);
	}

	protected void closeView() {
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
