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

package jmt.gui.common.panels;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

import jmt.framework.gui.components.HtmlPanel;
import jmt.framework.gui.components.JMTDialog;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.common.startScreen.GraphStartScreen;

/**
 * <p>Title: About Dialog Factory</p>
 * <p>Description: This class will create dialogs to show credits for each
 * application.</p>
 *
 * @author Bertoli Marco
 *         Date: 1-feb-2006
 *         Time: 16.42.10
 */
public class AboutDialogFactory {
	/**
	 * Authors of each application
	 */
	protected static String[] JMVA = { "Bertoli Marco", "Conti Andrea", "Dall'Orso Federico", "Omini Stefano", "Granata Federico" };

	protected static String[] JSIM = { "Bertoli Marco", "Granata Federico", "Omini Stefano", "Radaelli Francesco", "Dall'Orso Federico" };

	protected static String[] JMODEL = { "Bertoli Marco", "D'Aquino Francesco", "Granata Federico", "Omini Stefano", "Radaelli Francesco" };

	protected static String[] JABA = { "Bertoli Marco", "Zanzottera Andrea", "Gimondi Carlo" };

	protected static String[] JMCH = { "Canakoglu Arif", "Di Mauro Ernesto" };

	protected static String[] JWAT = { "Brambilla Davide", "Fumagalli Claudio" };

	/**
	 * Variables
	 */
	protected static final int BORDERSIZE = 20;
	protected static final String TITLE_START = "<html><font face=\"Verdana\" size=+4><b>";
	protected static final String TITLE_END = "</b></font></html>";

	protected static final String SEC_START = "<font size=+2><b>";
	protected static final String SEC_END = "</b></font>";

	protected static final String LEGAL = "<html><font size=\"2\">" + "  This program is free software; you can redistribute it and/or modify "
			+ "  it under the terms of the GNU General Public License as published by "
			+ "  the Free Software Foundation; either version 2 of the License, or " + "  (at your option) any later version." + "<br><br>"
			+ "  This program is distributed in the hope that it will be useful, "
			+ "  but WITHOUT ANY WARRANTY; without even the implied warranty of "
			+ "  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the " + "  GNU General Public License for more details."
			+ "</font></html>";
	protected static final String WEBSITE = "<b>Home Page:</b> <a href=\"http://jmt.sourceforge.net\">http://jmt.sourceforge.net</a>";

	protected static boolean initialized = false;
	protected static JPanel panel;
	protected static JLabel title, legal;
	protected static HtmlPanel textArea;
	protected static String text; // text to be displayed in textArea

	/**
	 * Initialize data structures and window layout.
	 */
	static {
		// Sorts arrays alphabetically
		Arrays.sort(JMVA);
		Arrays.sort(JSIM);
		Arrays.sort(JMODEL);
		Arrays.sort(JABA);
		Arrays.sort(JMCH);
		Arrays.sort(JWAT);

		// Initialize dialog layout
		panel = new JPanel(new BorderLayout(BORDERSIZE / 2, BORDERSIZE / 2));
		panel.setBorder(BorderFactory.createEmptyBorder(BORDERSIZE, BORDERSIZE, BORDERSIZE, BORDERSIZE));
		// Adds polimi image
		JPanel tmpPanel = new JPanel(new BorderLayout(BORDERSIZE, BORDERSIZE * 2));
		JLabel image = new JLabel();
		image.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_LOGOPOLI, new Dimension(75, 75)));
		image.setHorizontalAlignment(SwingConstants.CENTER);
		image.setVerticalAlignment(SwingConstants.CENTER);
		tmpPanel.add(image, BorderLayout.WEST);
		// Adds polimi description
		JLabel polimiLabel = new JLabel(GraphStartScreen.HTML_CONTENT_TITLE);
		polimiLabel.setVerticalAlignment(SwingConstants.CENTER);
		polimiLabel.setHorizontalAlignment(SwingConstants.CENTER);
		tmpPanel.add(polimiLabel, BorderLayout.CENTER);

		// Adds application title
		title = new JLabel();
		title.setHorizontalTextPosition(SwingConstants.RIGHT);
		title.setHorizontalAlignment(SwingConstants.CENTER);
		title.setIconTextGap(BORDERSIZE);
		tmpPanel.add(title, BorderLayout.SOUTH);

		panel.add(tmpPanel, BorderLayout.NORTH);

		// Adds text area
		textArea = new HtmlPanel();
		textArea.setAlignmentX(Component.CENTER_ALIGNMENT);
		textArea.setAlignmentY(Component.CENTER_ALIGNMENT);
		textArea.setOpaque(false);
		panel.add(textArea, BorderLayout.CENTER);

		legal = new JLabel(LEGAL);
		panel.add(legal, BorderLayout.SOUTH);

	}

	/**
	 * Creates a new modal JMTDialog with specified owner and with panel inside, displaying current text.
	 * @param owner owner of the dialog. If it's null or invalid, created dialog will not
	 * be modal
	 * @param title title of dialog to be created
	 * @return created dialog
	 */
	protected static JMTDialog createDialog(Window owner, String title) {
		final JMTDialog dialog;
		if (owner == null) {
			dialog = new JMTDialog();
		} else if (owner instanceof Dialog) {
			dialog = new JMTDialog((Dialog) owner, true);
		} else if (owner instanceof Frame) {
			dialog = new JMTDialog((Frame) owner, true);
		} else {
			dialog = new JMTDialog();
		}
		dialog.setTitle(title);
		dialog.getContentPane().setLayout(new BorderLayout());
		dialog.getContentPane().add(panel, BorderLayout.CENTER);

		// Sets text to be displayed
		textArea.setText("<html><p><font size=\"-1\">" + WEBSITE + "<br><br>" + text + "</font></p></html>");

		// Adds exit button
		JButton exit = new JButton();
		exit.setText("Close");
		exit.addActionListener(new ActionListener() {

			/**
			 * Invoked when an action occurs.
			 */
			public void actionPerformed(ActionEvent e) {
				dialog.close();
			}
		});

		JPanel bottom = new JPanel();
		bottom.add(exit);
		dialog.getContentPane().add(bottom, BorderLayout.SOUTH);
		dialog.centerWindow(450, 500);
		return dialog;
	}

	/**
	 * Add contributor names to current about window
	 * @param names names to be added
	 */
	private static void addNames(String[] names) {
		text = "<b>Major Contributors: </b>";
		for (int i = 0; i < names.length - 1; i++) {
			text += names[i] + ", ";
		}
		text += names[names.length - 1] + ".";
	}

	/**
	 * Shows JMVA about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJMVA(Window owner) {
		title.setText(TITLE_START + "JMVA" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JMVAICON, new Dimension(50, 50)));
		addNames(JMVA);
		createDialog(owner, "About JMVA").show();
	}

	/**
	 * Shows JSIM about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJSIM(Window owner) {
		title.setText(TITLE_START + "JSIM<em>wiz</em>" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JSIMICON, new Dimension(50, 50)));
		addNames(JSIM);
		createDialog(owner, "About JSIMwiz").show();
	}

	/**
	 * Shows JMODEL about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJMODEL(Window owner) {
		title.setText(TITLE_START + "JSIM<em>graph</em>" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JMODELICON, new Dimension(50, 50)));
		addNames(JMODEL);
		createDialog(owner, "About JSIMgraph").show();
	}

	/**
	 * Shows JABA about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJABA(Window owner) {
		title.setText(TITLE_START + "JABA" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JABAICON, new Dimension(50, 50)));
		addNames(JABA);
		createDialog(owner, "About JABA").show();
	}

	/**
	 * Shows JMCH about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJMCH(Window owner) {
		title.setText(TITLE_START + "JMCH" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JMCHICON, new Dimension(50, 50)));
		addNames(JMCH);
		createDialog(owner, "About JMCH").show();
	}

	/**
	 * Shows JWAT about window
	 * @param owner owner of this window (if null, window will not be modal)
	 */
	public static void showJWAT(Window owner) {
		title.setText(TITLE_START + "JWAT" + TITLE_END);
		title.setIcon(JMTImageLoader.loadImage(GraphStartScreen.IMG_JWATICON, new Dimension(50, 50)));
		addNames(JWAT);
		createDialog(owner, "About JWAT").show();
	}
}
