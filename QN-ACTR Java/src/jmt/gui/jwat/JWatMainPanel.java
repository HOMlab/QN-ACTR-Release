package jmt.gui.jwat;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingConstants;

import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.panels.AboutDialogFactory;
import jmt.gui.common.resources.JMTImageLoader;

public class JWatMainPanel extends WizardPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//Start screen image
	private static URL imageURL = JWatMainPanel.class.getResource("StartScreenJWat.png");
	//jWAT tool icons
	private String IMG_TRAFFIC_ICON = "TrafficIcon";
	private String IMG_SAVE_ICON = "Open";
	private String IMG_PATH_ICON = "PathIcon";
	private String IMG_FITTING_ICON = "FittingIcon";
	private String IMG_WL_ICON = "WorkLoadIcon";
	//Tool buttons size
	private static final int BUTTONSIZE = 25;
	//Rollover help
	private Rollover rollover = new Rollover();
	private HoverHelp help;
	private MainJwatWizard parent = null;

	protected AbstractAction startFitting = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Fitting");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_FITTING_ICON, new Dimension(BUTTONSIZE, BUTTONSIZE)));
		}

		public void actionPerformed(ActionEvent e) {
			parent.setFittingEnv("load");
		}
	};
	protected AbstractAction startWLA = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Worload analyzer");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_WL_ICON, new Dimension(BUTTONSIZE + 10, BUTTONSIZE + 10)));
		}

		public void actionPerformed(ActionEvent e) {
			parent.setWorkloadEnv("load");
		}
	};
	protected AbstractAction startTraffic = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Traffic Analysis  - Burstiness");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_TRAFFIC_ICON, new Dimension(BUTTONSIZE + 10, BUTTONSIZE + 10)));
		}

		public void actionPerformed(ActionEvent e) {
			parent.setTrafficEnv();
		}
	};
	protected AbstractAction startLoadWorkload = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Open a demo file for Workload Analysis");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_SAVE_ICON, new Dimension(BUTTONSIZE + 10, BUTTONSIZE + 10)));
		}

		public void actionPerformed(ActionEvent e) {
			parent.setWorkloadEnv("demo");
		}
	};
	protected AbstractAction startLoadFitting = new AbstractAction() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Open a demo file for fitting");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_SAVE_ICON, new Dimension(BUTTONSIZE + 10, BUTTONSIZE + 10)));
		}

		public void actionPerformed(ActionEvent e) {
			parent.setFittingEnv("demo");
		}
	};
	//Arrays of abstract actions for tool buttons
	private AbstractAction[] buttonAction = { startWLA, startFitting,  startTraffic, startLoadWorkload, startLoadFitting  };

	/**
	 * Helper method used to create a button inside a JPanel
	 * @param action action associated to that button
	 * @return created component
	 */
	private JComponent createButton(AbstractAction action) {
		JPanel panel = new JPanel(); // Use gridbag as centers by default
		JButton button = new JButton(action);
		button.setHorizontalTextPosition(SwingConstants.CENTER);
		button.setVerticalTextPosition(SwingConstants.BOTTOM);
		button.setPreferredSize(new Dimension((int) (BUTTONSIZE * 3.5), (BUTTONSIZE * 2)));
		button.addMouseListener(rollover);
		//if (action == buttonAction[4]) {
		//	button.setVisible(false);
		//}
		//if (action == buttonAction[0]) {
		//	button.setEnabled(false);
		//}
		//if(action == buttonAction[2]) button.setEnabled(false);
		//if(action == buttonAction[4]) button.setEnabled(false);
		panel.add(button);
		return panel;
	}

	/**
	 * This class is used to perform rollover on the buttons by changing background
	 */
	public class Rollover extends MouseAdapter {
		private Color normal;
		private Color rollover;

		public Rollover() {
			normal = new JButton().getBackground();
			rollover = new Color(83, 126, 126);
		}

		/**
		 * Invoked when the mouse enters a component.
		 */
		@Override
		public void mouseEntered(MouseEvent e) {
			((Component) e.getSource()).setBackground(rollover);
		}

		/**
		 * Invoked when the mouse exits a component.
		 */
		@Override
		public void mouseExited(MouseEvent e) {
			((Component) e.getSource()).setBackground(normal);
		}
	}

	public JWatMainPanel(MainJwatWizard parent) {
		this.parent = parent;
		this.help = parent.getHelp();
		this.setLayout(new BorderLayout());
		JPanel upper = new JPanel(new FlowLayout());
		JLabel upperLabel = new JLabel();
		upperLabel.setPreferredSize(new Dimension(300, 10));
		upper.add(upperLabel);

		JPanel bottom = new JPanel(new FlowLayout());
		JLabel bottomLabel = new JLabel();
		bottomLabel.setPreferredSize(new Dimension(300, 10));
		bottom.add(bottomLabel);

		this.add(upper, BorderLayout.NORTH);
		this.add(bottom, BorderLayout.SOUTH);

		JPanel eastPanel = new JPanel(new FlowLayout());
		eastPanel.add(Box.createVerticalStrut(5), BorderLayout.NORTH);
		JPanel buttonPanel = new JPanel(new GridLayout(buttonAction.length, 1, 2, 15));
		eastPanel.add(buttonPanel, BorderLayout.CENTER);
		for (AbstractAction element : buttonAction) {
			buttonPanel.add(createButton(element));
		}
		JLabel imageLabel = new JLabel();
		imageLabel.setBorder(BorderFactory.createEmptyBorder(BUTTONSIZE - 5, 1, 0, 0));
		//imageLabel.setIcon(new ImageIcon(image));
		imageLabel.setIcon(new ImageIcon(new ImageIcon(imageURL).getImage().getScaledInstance(400, 315, Image.SCALE_SMOOTH)));
		imageLabel.setHorizontalAlignment(SwingConstants.RIGHT);
		imageLabel.setVerticalAlignment(SwingConstants.NORTH);

		//JLabel description = new JLabel("<html><body><h3>This is a simple<br>descirption added to this<br>page. Please don't mind it<br>will be replaced soon</h3></body></html>");
		//this.add(description,BorderLayout.WEST);
		this.add(imageLabel, BorderLayout.CENTER);
		this.add(eastPanel, BorderLayout.EAST);
		makeToolbar();
		makeMenubar();
		parent.setEnableButton("Solve", false);
		parent.setActionButton("Help", new AbstractAction() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			public void actionPerformed(ActionEvent e) {
				System.err.println("HELP");
			}
		});
	}

	@Override
	public String getName() {
		return "Main panel";
	}

	private JToolBar workloadToolbar = null;

	/**
	 * @return the toolbar for the exact wizard. Shamelessly uses icon from the main jmt frame
	 */
	public void makeToolbar() {
		workloadToolbar = new JToolBar();
		workloadToolbar.setRollover(true);
		workloadToolbar.setOrientation(SwingConstants.HORIZONTAL);
		workloadToolbar.setFloatable(false);
		//null values add a gap between toolbar icons
		Action[] actions = { HELP_CREDITS };
		String[] icons = { "Help" };
		String[] htext = { "Show help" };
		JButton button;
		workloadToolbar.setBorderPainted(true);
		//i index scans actions' array which includes null values, while j scans other arrays.
		//so j must be decremented when a null value is found in action array.
		for (int i = 0, j = 0; i < actions.length; i++, j++) {
			if (actions[i] == null) {
				j--;
				workloadToolbar.addSeparator(new Dimension(20, 2));
			} else {
				button = new JButton(actions[i]);
				button.setText("");
				button.setIcon(JMTImageLoader.loadImage(icons[j]));
				button.setRolloverIcon(JMTImageLoader.loadImage(icons[j] + "RO"));
				button.setPressedIcon(JMTImageLoader.loadImage(icons[j] + "P"));
				button.setFocusPainted(false);
				button.setContentAreaFilled(false);
				button.setBorder(BorderFactory.createEmptyBorder(0, 0, 0, 0));
				workloadToolbar.add(button);
				help.addHelp(button, htext[j]);
			}
		}
		parent.setToolBar(workloadToolbar);
	}

	private AbstractAction HELP_CREDITS = new AbstractAction("About JWAT") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Credits");
		}

		public void actionPerformed(ActionEvent e) {
			AboutDialogFactory.showJWAT(parent);
		}
	};
	private JMenuBar workloadMenubar = null;

	/**
	 * Creates workload analysis menu
	 * @return menu
	 */
	public void makeMenubar() {
		workloadMenubar = new JMenuBar();
		JMenuItem[][] menuItems = { { new JMenuItem(HELP_CREDITS) } };
		String[] menuTitles = { "Help" };
		char[] chars = { 'e' };
		for (int i = 0; i < menuItems.length; i++) {
			JMenu menu = new JMenu(menuTitles[i]);
			menu.setMnemonic(chars[i]);
			for (int j = 0; j < menuItems[i].length; j++) {
				if (menuItems[i][j] == null) {
					menu.addSeparator();
				} else {
					menu.add(menuItems[i][j]);
				}
			}
			workloadMenubar.add(menu);
		}
		parent.setMenuBar(workloadMenubar);
	}

	@Override
	public void gotFocus() {
		if (parent.getModel() != null && parent.getModel().getMatrix() != null) {
			if (JOptionPane.showConfirmDialog(this, "This operation resets all data. Continue ?", "WARNING", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
				parent.getSession().resetSession();
				parent.resetScreen();
				((JWatWizard) getParentWizard()).setEnableButton("Solve", false);
			} else {
				parent.setLastPanel();
			}
		} else {

		}
	}
}
