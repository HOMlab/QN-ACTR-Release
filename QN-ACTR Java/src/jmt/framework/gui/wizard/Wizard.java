/**    
  * Copyright (C) 2006, Laboratorio di Valutazione delle Prestazioni - Politecnico di Milano

  * This program is free software; you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation; either version 2 of the License, or
  * (at your option) any later version.

  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.

  * You should have received a copy of the GNU General Public License
  * along with this program; if not, write to the Free Software
  * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
  */

package jmt.framework.gui.wizard;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import jmt.framework.gui.components.JMTFrame;
import jmt.framework.gui.listeners.AbstractJMTAction;

/**

 * @author alyf (Andrea Conti)
 * Date: 11-set-2003
 * Time: 17.38.00

 */
//Modified by Federico Dall'Orso
/**
 * a generic Wizard framework
 */
public class Wizard extends JMTFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/* button Actions */
	protected AbstractJMTAction ACTION_NEXT = new AbstractJMTAction("Next >") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setAcceleratorKey(KeyEvent.VK_RIGHT, ActionEvent.CTRL_MASK);
			setTooltipText("Go to next screen");
		}

		public void actionPerformed(ActionEvent e) {
			showNext();
		}
	};

	protected AbstractJMTAction ACTION_PREV = new AbstractJMTAction("< Back") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setAcceleratorKey(KeyEvent.VK_LEFT, ActionEvent.CTRL_MASK);
			setTooltipText("Go to previous screen");
		}

		public void actionPerformed(ActionEvent e) {
			showPrev();
		}
	};

	protected AbstractJMTAction ACTION_CANCEL = new AbstractJMTAction("Cancel") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setTooltipText("Exit discarding all changes");
		}

		public void actionPerformed(ActionEvent e) {
			close();
		}
	};

	protected AbstractJMTAction ACTION_FINISH = new AbstractJMTAction("Solve") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setTooltipText("Solve this Model");
		}

		public void actionPerformed(ActionEvent e) {
			if (checkFinish()) {
				finish();
			}
		}
	};

	protected AbstractJMTAction ACTION_HELP = new AbstractJMTAction("Help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			setTooltipText("Show quick help screen");
		}

		public void actionPerformed(ActionEvent e) {
			help();
		}
	};

	protected JTabbedPane tabbedPane;
	protected JComponent buttons;

	protected WizardPanel currentPanel = null;
	protected int currentIndex;
	protected int panelCount;

	protected List<WizardPanel> panels;

	public Wizard() {
		super(true);
		panels = new ArrayList<WizardPanel>();
		currentIndex = -1;
		panelCount = 0;

		tabbedPane = makeTabbedPane();
		buttons = makeButtons();

		Container cp = getContentPane();
		cp.setLayout(new BorderLayout());
		cp.add(tabbedPane, BorderLayout.CENTER);
		cp.add(buttons, BorderLayout.SOUTH);
		updateActions();
	}

	public Wizard(String title) {
		this();
		setTitle(title);
	}

	public Wizard(String title, Image icon) {
		this(title);
		this.setIconImage(icon);
	}

	/**
	 * @return the button panel
	 */
	protected JComponent makeButtons() {
		JButton button_finish = new JButton(ACTION_FINISH);
		JButton button_cancel = new JButton(ACTION_CANCEL);
		JButton button_next = new JButton(ACTION_NEXT);
		JButton button_previous = new JButton(ACTION_PREV);
		JButton button_help = new JButton(ACTION_HELP);
		JPanel res = new JPanel();
		res.add(button_previous);
		res.add(button_next);
		res.add(button_finish);
		res.add(button_cancel);
		//res.add(Box.createHorizontalStrut(80));
		res.add(button_help);
		return res;
	}

	protected JTabbedPane makeTabbedPane() {
		return new JTabbedPane() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			/* we hook into the tabbedpane's tab switching method and make the change
			only if the current panel agrees */
			@Override
			public void setSelectedIndex(int index) {
				int oldIndex = model.getSelectedIndex();
				if (index != oldIndex) { //ignore useless requests
					if (currentPanel != null) {
						if (index > oldIndex) {
							if (!currentPanel.canGoForward()) {
								return;
							}
						} else {
							if (!currentPanel.canGoBack()) {
								return;
							}
						}
						currentPanel.lostFocus();
					}
					super.setSelectedIndex(index);
					currentIndex = model.getSelectedIndex();
					currentPanel = (WizardPanel) getComponentAt(currentIndex);
					if (oldIndex >= 0 && oldIndex < getTabCount()) {
						WizardPanel oldPanel = (WizardPanel) getComponentAt(oldIndex);
						setTitleAt(oldIndex, oldPanel.getName());
					}
					setTitleAt(index, "<html><body align=\"center\"><font color=\"000000\"><b>" + currentPanel.getName()
							+ "</b></font></body></html>");
					currentPanel.gotFocus();
					updateActions();
				}
			}
		};
	}

	protected boolean checkFinish() {
		if (currentPanel != null) {
			currentPanel.lostFocus();
		}
		for (int i = 0; i < panelCount; i++) {
			if (!panels.get(i).canFinish()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Called when the "Finish" button is pressed. Must be overridden in subclasses!
	 */
	protected void finish() {

		currentPanel.lostFocus();
		dispose();
	}

	/**
	 * Called when the "Cancel" button is pressed. Should be overridden in subclasses
	 * @return true if window can be closed, false otherwise.
	 */
	protected boolean cancel() {
		return true;
	}

	/* (non-Javadoc)
	 * @see jmt.framework.gui.components.JMTFrame#canBeClosed()
	 */
	@Override
	public boolean canBeClosed() {
		return cancel();
	}

	private void help() {
		currentPanel.help();
	}

	protected void showNext() {
		if (currentIndex == -1) {
			return;
		}
		if (currentIndex == panels.size() - 1) {
			return; //should not happen anyway
		}
		tabbedPane.setSelectedIndex(currentIndex + 1);
	}

	protected void showPrev() {
		if (currentIndex == -1) {
			return;
		}
		if (currentIndex == 0) {
			return; //should not happen anyway
		}
		tabbedPane.setSelectedIndex(currentIndex - 1);
	}

	/**
	 * enables/disables buttons according to current status
	 */
	protected void updateActions() {
		ACTION_NEXT.setEnabled(currentIndex >= 0 && currentIndex < panelCount - 1);
		ACTION_PREV.setEnabled(currentIndex > 0);
		ACTION_CANCEL.setEnabled(true);
		ACTION_FINISH.setEnabled(panelCount > 0);
		ACTION_HELP.setEnabled(panelCount > 0);
	}

	/**
	 * Add a new WizardPanel to the Wizard
	 */
	public void addPanel(WizardPanel p) {
		p.setParentWizard(this);
		panels.add(p);
		tabbedPane.add(p.getName(), p);
		panelCount = tabbedPane.getTabCount();
		if (panelCount == 1) {
			tabbedPane.setSelectedIndex(0);
		}
		updateActions();
	}

	/**
	 * Add a new WizardPanel to the Wizard
	 */
	public void addPanel(WizardPanel p, int index) {
		p.setParentWizard(this);
		panels.add(p);
		tabbedPane.insertTab(p.getName(), null, p, "", index);
		panelCount = tabbedPane.getTabCount();
		if (panelCount == 1) {
			tabbedPane.setSelectedIndex(0);
		} else {
			tabbedPane.setSelectedIndex(index - 1);
			tabbedPane.setSelectedIndex(index);
		}
		updateActions();
	}

	/**
	  * Removes a WizardPanel to the Wizard
	  */
	public void removePanel(WizardPanel p) {
		tabbedPane.remove(p);
		panelCount = tabbedPane.getTabCount();
		panels.remove(p);
		updateActions();
	}
}
