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
package jmt.gui.jwat;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.net.URL;
import java.util.Locale;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import jmt.framework.gui.controller.Manager;
import jmt.gui.common.resources.JMTImageLoader;

public class JWatStartScreen extends JFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	//Start screen image
	private static URL imageURL = JWatStartScreen.class.getResource("StartScreenJWat.png");
	//jWAT tool icons
	private String IMG_JWATICON = "JWATIcon";
	private String IMG_TRAFFIC_ICON = "TrafficIcon";
	private String IMG_SAVE_ICON = "Open";
	private String IMG_PATH_ICON = "PathIcon";
	private String IMG_WL_ICON = "WorkLoadIcon";
	//Tool buttons size
	private static final int BUTTONSIZE = 25;
	//Rollover help
	private Rollover rollover = new Rollover();

	protected AbstractAction startPath = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Path analyzer");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_PATH_ICON, new Dimension(BUTTONSIZE, BUTTONSIZE)));
		}

		public void actionPerformed(ActionEvent e) {
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
			//			WorkloadAnalysisWizard.main(new String[]{"load"});
			JWatStartScreen.this.dispose();
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
			//BurstWizard.main(null);
			JWatStartScreen.this.dispose();
		}
	};
	protected AbstractAction startLoad = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Open a demo file");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage(IMG_SAVE_ICON, new Dimension(BUTTONSIZE + 10, BUTTONSIZE + 10)));
		}

		public void actionPerformed(ActionEvent e) {
			//			WorkloadAnalysisWizard.main(new String[]{"demo"});
			JWatStartScreen.this.dispose();
		}
	};
	protected AbstractAction empty = new AbstractAction() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
		}

		public void actionPerformed(ActionEvent e) {
		}
	};
	//Arrays of abstract actions for tool buttons
	private AbstractAction[] buttonAction = { startPath, startWLA, startTraffic, empty, startLoad };

	/**
	 * jWAT Constructor.
	 */
	public JWatStartScreen() {
		initGUI();
		addListeners();
	}

	/*
	 * Initializes jWAT start screen GUI
	 */
	private void initGUI() {
		this.setIconImage(JMTImageLoader.loadImage(IMG_JWATICON).getImage());
		this.setResizable(false);
		this.setTitle("jWAT");
		this.setSize(520, 400);
		//Image image = new ImageIcon(imageURL).getImage();
		//image = image.getScaledInstance(400, 315, Image.SCALE_SMOOTH);
		JPanel eastPanel = new JPanel(new BorderLayout());
		eastPanel.add(Box.createVerticalStrut(5), BorderLayout.NORTH);
		JPanel buttonPanel = new JPanel(new GridLayout(buttonAction.length, 1, 2, 2));
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
		this.getContentPane().add(imageLabel, BorderLayout.CENTER);
		this.getContentPane().add(eastPanel, BorderLayout.EAST);
	}

	/*
	 * Add all listeners to start screen 
	 */
	private void addListeners() {
		this.addWindowListener(new WindowAdapter() {
			/**
			 * Invoked when a window is in the process of being closed.
			 */
			@Override
			public void windowClosing(WindowEvent e) {
				Manager.exit(JWatStartScreen.this);
			}

			/**
			 * Invoked when a window has been closed.
			 */
			@Override
			public void windowClosed(WindowEvent e) {
				Manager.exit(JWatStartScreen.this);
			}
		});
	}

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
		if (action == buttonAction[3]) {
			button.setVisible(false);
		}
		if (action == buttonAction[0]) {
			button.setEnabled(false);
		}
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

	/**
	 * Main method.
	 * @param args no args.
	 */
	public static void main(String[] args) {
		new JWatStartScreen().setVisible(true);
	}
}
