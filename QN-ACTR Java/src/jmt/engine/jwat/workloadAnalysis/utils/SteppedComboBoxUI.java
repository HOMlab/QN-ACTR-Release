package jmt.engine.jwat.workloadAnalysis.utils;

import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.plaf.basic.BasicComboPopup;
import javax.swing.plaf.basic.ComboPopup;

import com.jgoodies.looks.plastic.PlasticComboBoxUI;

public class SteppedComboBoxUI extends PlasticComboBoxUI {
	@Override
	protected ComboPopup createPopup() {
		BasicComboPopup popup = new BasicComboPopup(comboBox) {

			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public void show() {
				Dimension popupSize = ((SteppedComboBox) comboBox).getPopupSize();
				popupSize.setSize(popupSize.width, getPopupHeightForRowCount(comboBox.getMaximumRowCount()));
				Rectangle popupBounds = computePopupBounds(0, comboBox.getBounds().height, popupSize.width, popupSize.height);
				scroller.setMaximumSize(popupBounds.getSize());
				scroller.setPreferredSize(popupBounds.getSize());
				scroller.setMinimumSize(popupBounds.getSize());
				list.invalidate();
				int selectedIndex = comboBox.getSelectedIndex();
				if (selectedIndex == -1) {
					list.clearSelection();
				} else {
					list.setSelectedIndex(selectedIndex);
				}
				list.ensureIndexIsVisible(list.getSelectedIndex());
				setLightWeightPopupEnabled(comboBox.isLightWeightPopupEnabled());

				show(comboBox, popupBounds.x, popupBounds.y);
			}
		};
		popup.getAccessibleContext().setAccessibleParent(comboBox);
		return popup;
	}
}