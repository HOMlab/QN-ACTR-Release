package jmt.engine.jwat.workloadAnalysis.utils;

import java.awt.Dimension;
import java.awt.Graphics;
import java.util.Vector;

import javax.swing.ComboBoxModel;
import javax.swing.JComboBox;

public class SteppedComboBox extends JComboBox {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected int popupWidth;

	public SteppedComboBox(ComboBoxModel aModel) {
		super(aModel);
		setUI(new SteppedComboBoxUI());
		popupWidth = 0;
	}

	public SteppedComboBox(final Object[] items) {
		super(items);
		setUI(new SteppedComboBoxUI());
		popupWidth = 0;
	}

	public SteppedComboBox(Vector items) {
		super(items);
		setUI(new SteppedComboBoxUI());
		popupWidth = 0;
	}

	public void setPopupWidth(int width) {
		Dimension size = getSize();
		Graphics g = this.getGraphics();
		popupWidth = size.width;
		for (int i = 0; i < dataModel.getSize(); i++) {
			if (g.getFontMetrics().stringWidth((String) dataModel.getElementAt(i)) > popupWidth) {
				popupWidth = g.getFontMetrics().stringWidth((String) dataModel.getElementAt(i));
			}
		}
		g.dispose();
	}

	public Dimension getPopupSize() {
		Dimension size = getSize();
		Graphics g = this.getGraphics();
		popupWidth = size.width;
		for (int i = 0; i < dataModel.getSize(); i++) {
			if (g.getFontMetrics().stringWidth((String) dataModel.getElementAt(i)) > popupWidth) {
				popupWidth = g.getFontMetrics().stringWidth((String) dataModel.getElementAt(i));
			}
		}
		g.dispose();
		return new Dimension(popupWidth + 5, size.height);
	}
}
