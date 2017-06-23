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
package jmt.gui.jwat.trafficAnalysis.panels;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ScrollPaneConstants;

import jmt.engine.jwat.trafficAnalysis.BurstEngine;
import jmt.engine.jwat.trafficAnalysis.ModelTrafficAnalysis;
import jmt.engine.jwat.trafficAnalysis.OnResetModel;
import jmt.engine.jwat.trafficAnalysis.OnSetParamtersListener;
import jmt.engine.jwat.trafficAnalysis.TrafficAnalysisSession;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.MainJwatWizard;

/**
 * <p>Title: Sectors Textual Panel</p>
 * <p>Description: This panel will show textual results</p>
 *
 * @author Marco Rosini
 *         Date: 829-07-2006
 *         Time: 11.43.34
 */
public class TextualPanel extends WizardPanel implements JWATConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public static final String DESCRIPTION_PANEL = "<html><body align=\"left\"><font size=\"4\"><b>Burstiness results</b>"
			+ "</font><font size=\"3\"><br>All the burstiness values calculated. Click the button to save it in HTML format.</body></html>";

	//GUI components
	private JEditorPane synView;
	private JScrollPane synScroll;
	private JButton button_save;

	//data source
	private BurstEngine burstengine;
	private MainJwatWizard burstwizard;

	protected AbstractAction ACTION_SAVE = new AbstractAction("  Save  ") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(SHORT_DESCRIPTION, "Save the results");
		}

		public void actionPerformed(ActionEvent e) {
			save();
		}
	};

	public TextualPanel(MainJwatWizard burstwizard) {
		super();
		this.burstwizard = burstwizard;
		((TrafficAnalysisSession) this.burstwizard.getSession()).addSetParamsListener(new OnSetParamtersListener() {
			public void ParamsSetted() {
				burstengine = ((TrafficAnalysisSession) TextualPanel.this.burstwizard.getSession()).getEngine();
				TextualPanel.this.removeAll();
				initComponents();
			}
		});
		((ModelTrafficAnalysis) this.burstwizard.getModel()).addResetModelListener(new OnResetModel() {
			public void modelResetted() {
				TextualPanel.this.removeAll();
			}
		});
	}

	private void initComponents() {
		Box vBox = Box.createVerticalBox();
		Box hBox = Box.createHorizontalBox();
		Box h2Box = Box.createHorizontalBox();
		JLabel descrLabel = new JLabel(DESCRIPTION_PANEL);
		button_save = new JButton(ACTION_SAVE);
		button_save.setEnabled(false);

		synView = new JTextPane();
		synView.setContentType("text/html");
		synView.setEditable(false);

		synScroll = new JScrollPane(synView);
		synScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		synScroll.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(h2Box);
		vBox.add(Box.createVerticalStrut(20));
		vBox.add(synScroll);
		vBox.add(Box.createVerticalStrut(20));
		//h2Box.add(Box.createHorizontalStrut(20));
		h2Box.add(descrLabel);
		h2Box.add(button_save);
		hBox.add(Box.createHorizontalStrut(20));
		hBox.add(vBox);
		hBox.add(Box.createHorizontalStrut(20));
		// h2Box.setMaximumSize(new Dimension(580,50));
		this.setLayout(new GridLayout(1, 1));
		this.add(hBox);
		synView
				.setText("<html><body><center><font face=\"bold\" size=\"3\">Burstiness values will be here displayed once you solve the problem.</font></center></body></html>");
	}

	@Override
	public String getName() {
		return "Burst Values - Text";
	}

	@Override
	public void gotFocus() {
		redraw();
		repaint();
	}

	/**
	 * Print the textual values of the burstiness factors
	 */
	public void redraw() {

		//            if (burstwizard.getResultOk()) {
		double[] resultB = burstengine.getData("b");
		double[] resultA = burstengine.getData("a");
		double[] ArrPlusCount = burstengine.getData("ArrPlusCount");
		double[] ArrMinusCount = burstengine.getData("ArrMinusCount");
		double[] LambdaPlus = burstengine.getData("lambdaPlus");
		double[] ArrPlus = burstengine.getData("ArrPlus");
		double[] lambda = burstengine.getData("lambda");
		String res = "<html><body align=\"Left\">";
		for (int i = 1; i < (resultB).length; i++) {
			res = res + "<b><i>Epoch n° = " + (i + 1) + "</i></b><br><br>";
			res = res + " b = " + resultB[i] + "<br>";
			res = res + " a = " + resultA[i] + "<br>";
			res = res + " lambda = " + lambda[0] + "<br>";
			res = res + " Number of epochs for which lambda(k) > lambda = " + ArrPlus[i] + "<br>";
			res = res + " Arr+ = " + ArrPlusCount[i] + "<br>";
			res = res + " Arr- = " + ArrMinusCount[i] + "<br>";
			res = res + " Lambda+ = " + LambdaPlus[i] + "<br>";
			res = res + " L = " + (ArrPlusCount[i] + ArrMinusCount[i]) + "<br>";
			res = res.concat("<br><br>");
		}
		res = res.concat("</body>" + "</html>");
		button_save.setEnabled(true);
		synView.setText(res);

		//                   }
		//            else {
		//               synView.setText("<html><body><center><font face=\"bold\" size=\"3\">Burstiness values will be here displayed once you solve the problem.</font></center></body></html>");
		//            }
	}

	private void save() {
		FileFilter HTMLfilter = new FileFilter(".html", "Html");
		FileChooser fileChooser = new FileChooser(HTMLfilter);
		fileChooser.setFileFilter(HTMLfilter);
		//fileChooser.addChoosableFileFilter(XMLfilter);
		int r = fileChooser.showSaveDialog(TextualPanel.this);
		if (r == JFileChooser.APPROVE_OPTION) {
			File file = fileChooser.getSelectedFile();
			if (fileChooser.getFileFilter().equals(HTMLfilter)) {
				try {

					String Title = new String("<font size=\"4\"><b>Burstiness Results</b><br><br>");
					String temp = new String(synView.getText());
					OutputStreamWriter fileStream = new OutputStreamWriter(new FileOutputStream(file));
					fileStream.write(Title);
					fileStream.write(temp);
					fileStream.flush();
					fileStream.close();
				} catch (FileNotFoundException fnf) {
					JOptionPane.showMessageDialog(fileChooser, "File not found", "JMT - Error", JOptionPane.ERROR_MESSAGE);
				} catch (IOException ioe) {
					JOptionPane.showMessageDialog(fileChooser, "I/O exception", "JMT - Error", JOptionPane.ERROR_MESSAGE);
				}
			}

		}

	}

	//  --- Methods to choose a file -------------------------------------------------------------

	/**
	 * Custom file chooser class
	 */
	protected static class FileChooser extends JFileChooser {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected FileFilter defaultFilter;

		/**
		 * Creates a File chooser in the appropriate directory user deafault.
		 * @param defaultFilter default file filter
		 */
		public FileChooser(FileFilter defaultFilter) {
			super(new File(System.getProperty("user.dir")));
			this.defaultFilter = defaultFilter;
		}

		/**
		 * Overrides default method to provide a warning if saving over an existing file
		 */
		@Override
		public void approveSelection() {
			// Gets the choosed file name
			String name = getSelectedFile().getName();
			String parent = getSelectedFile().getParent();
			if (getDialogType() == OPEN_DIALOG) {
				super.approveSelection();
			}
			if (getDialogType() == SAVE_DIALOG) {
				try {
					FileFilter used = ((FileFilter) this.getFileFilter());

					if (!name.toLowerCase().endsWith(used.getExtension())) {
						name = name + used.getExtension();
						setSelectedFile(new File(parent, name));
					}
					if (getSelectedFile().exists()) {
						int resultValue = JOptionPane.showConfirmDialog(this, "<html>File <font color=#0000ff>" + name
								+ "</font> already exists in this folder.<br>Do you want to replace it?</html>", "JMT - Warning",
								JOptionPane.OK_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE);
						if (resultValue == JOptionPane.OK_OPTION) {
							getSelectedFile().delete();
							super.approveSelection();
						}
					} else {
						super.approveSelection();
					}
				} catch (Exception e) {
					JOptionPane.showMessageDialog(this, "Select a type for the output file", "Select a type for the output file",
							JOptionPane.ERROR_MESSAGE);
				}
			}
		}
	}

	/**
	 * Inner class used to create simple file filters with only extension check
	 */
	protected static class FileFilter extends javax.swing.filechooser.FileFilter {
		private String extension, description;

		/**
		 * Creates a new filefilter with specified extension and description
		 * @param extension extension of this filter (for example ".jmt")
		 * @param description description of this filter
		 */
		public FileFilter(String extension, String description) {
			this.extension = extension;
			this.description = description;
		}

		/**
		 * Whether the given file is accepted by this filter.
		 */
		@Override
		public boolean accept(File f) {
			String name = f.getName().toLowerCase();
			return name.endsWith(extension) || f.isDirectory();
		}

		/**
		 * The description of this filter
		 * @see javax.swing.filechooser.FileView#getName
		 */
		@Override
		public String getDescription() {
			return description + " (*" + extension + ")";
		}

		/**
		 * Gets extension of this filter
		 * @return extension of this filter
		 */
		public String getExtension() {
			return extension;
		}

	}

	// --------------------------------------------------------------------
	@Override
	public void lostFocus() {
		burstwizard.setLastPanel(TRAFFIC_TEXTUAL_PANEL);
	}

}
