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
package jmt.gui.jwat.workloadAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;

import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.VariableString;
import jmt.engine.jwat.filters.FilterOnData;
import jmt.engine.jwat.filters.FilterOnNumeric;
import jmt.engine.jwat.filters.FilterOnString;
import jmt.engine.jwat.filters.IntervalFilter;
import jmt.engine.jwat.filters.RandomFilter;
import jmt.engine.jwat.filters.TrimmingBetweenFilter;
import jmt.engine.jwat.filters.TrimmingFilter;
import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.exceptions.TrasformException;
import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.engine.jwat.workloadAnalysis.utils.SetMatrixListener;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.JWatWizard;
import jmt.gui.jwat.MainJwatWizard;
import jmt.gui.jwat.workloadAnalysis.chart.DispMatrix;
import jmt.gui.jwat.workloadAnalysis.chart.DispQQPlotMatrix;
import jmt.gui.jwat.workloadAnalysis.chart.QQPlotPreviewPanel;
import jmt.gui.jwat.workloadAnalysis.chart.SmallPlotDistGraph;
import jmt.gui.jwat.workloadAnalysis.tables.JWatBivariateStatsTable;
import jmt.gui.jwat.workloadAnalysis.tables.JWatBivariateStatsTableModel;
import jmt.gui.jwat.workloadAnalysis.tables.JWatUnivariateStatsTable;
import jmt.gui.jwat.workloadAnalysis.tables.JWatUnivariateStatsTableModel;

public class StatsPanel extends WizardPanel implements CommonConstants, JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//UPDATE 15/11/2006
	private boolean isSampled = false;

	// Tabbed pane statistics panel, univariate, bivariate, sampling operation and matrix
	private JTabbedPane statisticsTabbed = new JTabbedPane();
	// Univariate statistics panel
	private JPanel uniStatsPanel = null;
	// Bivariate statistics panel
	private JPanel bivStatsPanel = null;
	// Scroll pane statistics bivariate
	private ScrollBivariatePanel panelBiv = null;

	private JPanel samplePanel = null;
	private JLabel sampleDescr = null;
	private JPanel scatterMatrixPanel = null;
	private JPanel optionSampling = null;

	/* UPD */private JPanel scatterQQPlot = null;
	private DispQQPlotMatrix qqMatrix = null;

	//Help Strings
	private static String STATS_TABLE = "Click to select variable and see graphs and transofmrations applyable";
	private static String TRANSF_PANEL = "Select transformation to apply or undo a previously transformation";
	private static String TRANSF_COMBO = "Select transformation to apply";

	// Descrizione del pannello unviariaet stats
	public final static String UNIV_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Univariate statistics" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This panel shows univariate statistics and graphs" + HTML_FONT_NOR_END + HTML_END;

	// Descrizione del pannello bivariate stats
	public final static String BIVARIATE_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Bivariate statistics" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This panel shows the correlation coefficient" + HTML_FONT_NOR_END + HTML_END;

	// Descrizione del pannello di sampling e descrizioni dei sampling
	public final static String SAMPLING_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Sample construction (<I>Optional</I>)" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Allows the construction of a sample extracted from observations" + HTML_FONT_NOR_END + HTML_END;

	// Descrizione del pannello degli scatterplot
	public final static String MATRIX_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Scatter plot matrix" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This panel show scatter plot matrix of all variables. Double click on a scatter plot to open it in a new panel " + HTML_FONT_NOR_END
			+ HTML_END;

	// Descrizione del pannello degli scatterplot QQ
	public final static String QQ_MATRIX_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "QQ-plot matrix" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This panel show QQ-plot matrix of all variables. Double click on a QQ-plot to open it in a new panel " + HTML_FONT_NOR_END + HTML_END;

	public final static String SAMPLING_TRIMMING_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Trimming Sampling<p>description" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "This operation allows to remove<p>undesired data from source<p>observations" + " specifying<p>percentiles."/*from<p>which or to remove<p>observations"*/
			+ HTML_FONT_NOR_END + HTML_END;

	public final static String SAMPLING_RANDOM_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Random Sampling<p>description" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Specify the number of<p>" + "observations to be<p>" + "considered." + HTML_FONT_NOR_END + HTML_END;

	public final static String SAMPLING_INTERVAL_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Observ. # interval<p>description" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Specify the start ID and<p>" + "end ID of observations<p>" + "to be considered." + HTML_FONT_NOR_END + HTML_END;

	public final static String SAMPLING_FILTERING_DESCRITPION = HTML_START + HTML_FONT_TITLE + "Interval Sampling<p>description" + HTML_FONT_TIT_END
			+ HTML_FONT_NORM + "Specify observations to be<p>" + "considered specifying interval<p>" + "values (date and number type) or<p>"
			+ "substring (string type)." + HTML_FONT_NOR_END + HTML_END;

	public final static String SAMPLING_TRIMMING_PANEL_DESC = HTML_START
			+ HTML_FONT_TITLE
			+ "WARNING"
			+ HTML_FONT_TIT_END
			+ HTML_FONT_NORM
			+ "At this time you can apply any number of sampling operations and transformations to data with the constraint that you can't apply any transformation after a sampling;"
			+ "if you undo a transformation, applied before a sampling, this completely resets the sample; "
			+ "if you undo a sampling it completely resets the sample." + HTML_FONT_NOR_END + HTML_END;

	public final static String TRANSF_BORDER_TEXT = "Variable: ";

	public final static String TRANSF_LABEL_APPLIED = "Transformations applied to variable ";

	public final static String SAMPLING_LABEL_PANEL = "Option of the sampling operation";

	// Table bivariate statistics and model
	private JWatBivariateStatsTable tableBivariate;

	private JWatBivariateStatsTableModel modelBivariate;

	private ModelWorkloadAnalysis model = null;
	private WorkloadAnalysisSession session = null;

	// Add apply transformatio Action
	protected AbstractAction applyTranformation = new AbstractAction("Apply transformation") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Apply transformation to current selected variable");
		}

		// Removes all rows from inputTable and update varchooser comboBox and option panel
		public void actionPerformed(ActionEvent arg0) {
			// Check if selection is correctly made
			if (variableTable.getSelectedRow() >= 0) {
				if (!isSampled) {
					int curr = variableTable.getSelectedRow();
					if (((String) transfs.getSelectedItem()).equals("Logarithmic")) {
						try {
							model.doTransformationOnVariable(variableTable.getSelectedRow(), VariableNumber.LOGARITHMIC);
						} catch (TrasformException e) {
							JOptionPane.showMessageDialog(null,
									"It's impossible to apply logaritmic transformation due to the presence of 0 or negative values", "Erroe",
									JOptionPane.INFORMATION_MESSAGE);
						}
					}
					if (((String) transfs.getSelectedItem()).equals("Mix - Max")) {
						try {
							model.doTransformationOnVariable(variableTable.getSelectedRow(), VariableNumber.MINMAX);
						} catch (TrasformException e) {
							JOptionPane.showMessageDialog(null, e.getMsg(), "Erroe", JOptionPane.INFORMATION_MESSAGE);
						}
					}
					if (((String) transfs.getSelectedItem()).equals("Standard Deviation")) {
						try {
							model.doTransformationOnVariable(variableTable.getSelectedRow(), VariableNumber.STDEV);
						} catch (TrasformException e) {
							JOptionPane.showMessageDialog(null, e.getMsg(), "Erroe", JOptionPane.INFORMATION_MESSAGE);
						}
					}
					variableTable.setRowSelectionInterval(curr, curr);
				} else {
					JOptionPane.showMessageDialog(StatsPanel.this, "At least one sampling has been applied, undo it and proceed.", "Warinig",
							JOptionPane.WARNING_MESSAGE);
				}
			}
		}
	};

	// Add undo transformation Action
	protected AbstractAction undoTranformation = new AbstractAction("Undo transformation") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		{
			putValue(Action.SHORT_DESCRIPTION, "Undo previous transformation (if one) to current selected variable");
		}

		// Removes all rows from inputTable and update varchooser comboBox and option panel
		public void actionPerformed(ActionEvent arg0) {
			// Check if selection is correctly made
			if (variableTable.getSelectedRow() >= 0) {
				int curr = variableTable.getSelectedRow();
				if (model.undoTransformationOnVariable(variableTable.getSelectedRow())) {
					undoSam.setEnabled(false);
					isSampled = false;
				}
				variableTable.setRowSelectionInterval(curr, curr);
			}
		}
	};
	private HoverHelp help = null;
	private MainJwatWizard parent;

	/**
	 * Constructor, creates statistics panel and adds four panel of statistics
	 * information
	 * 
	 * @param parent
	 *            model of workload analysis
	 */
	public StatsPanel(MainJwatWizard parent) {
		this.parent = parent;
		model = (ModelWorkloadAnalysis) parent.getModel();
		session = (WorkloadAnalysisSession) parent.getSession();
		help = parent.getHelp();
		model.addOnSetMatrixObservationListener(new SetMatrixListener() {
			public void onSetMatrixObservation() {
				((JWatUnivariateStatsTableModel) variableTable.getModel()).setMatrixObs(model.getMatrix());
				variableTable.tableChanged(new TableModelEvent(variableTable.getModel()));
				variableTable.setRowSelectionInterval(0, 0);
				((JWatBivariateStatsTableModel) tableBivariate.getModel()).setMatrixObs(model.getMatrix());
				tableBivariate.tableChanged(new TableModelEvent(tableBivariate.getModel()));
				panelBiv.setNames();
				dispPanel.setModel(model);
				qqMatrix.setModel(model);
				varsList.setListData(model.getMatrix().getVariableNames());
				varsList.setSelectedIndex(0);
				samplingList.clearSelection();
				samplingList.setSelectedIndex(0);
			}

			public void onResetMatrixObservation() {
				((JWatUnivariateStatsTableModel) variableTable.getModel()).setMatrixObs(model.getMatrix());
				variableTable.tableChanged(new TableModelEvent(variableTable.getModel()));
				((JWatBivariateStatsTableModel) tableBivariate.getModel()).setMatrixObs(model.getMatrix());
				tableBivariate.tableChanged(new TableModelEvent(tableBivariate.getModel()));
				panelBiv.setNames();
				dispPanel.setModel(model);
				qqMatrix.setModel(model);
				varsList.setListData(new String[0]);
				samplingList.clearSelection();
				samplingList.setSelectedIndex(0);
			}
		});
		this.setLayout(new GridLayout(1, 1));
		this.add(statisticsTabbed);
		// Add single panel to tabbed pane
		uniStatsPanel = new JPanel(new GridLayout(1, 1));
		samplePanel = new JPanel(new BorderLayout());
		bivStatsPanel = new JPanel(new GridLayout(1, 1));
		scatterMatrixPanel = new JPanel(new GridLayout(1, 1));
		/* UPD */scatterQQPlot = new JPanel(new GridLayout(1, 1));
		createUnivariate();
		createSampling();
		createBivariate();
		createMatrix();
		/* UPD */createMatrixQQ();
		statisticsTabbed.addTab("Univariate", uniStatsPanel);
		statisticsTabbed.addTab("Sample Construction", samplePanel);
		statisticsTabbed.addTab("Bivariate", bivStatsPanel);
		statisticsTabbed.addTab("QQ-plot Matrix", scatterQQPlot);
		statisticsTabbed.addTab("Scatter plot Matrix", scatterMatrixPanel);
	}

	private void createMatrixQQ() {
		Box mainBox = Box.createVerticalBox();
		Box descBox = Box.createHorizontalBox();
		Box tableBox = Box.createHorizontalBox();

		scatterQQPlot.add(mainBox);

		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(descBox);
		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(tableBox);
		mainBox.add(Box.createVerticalStrut(10));

		descBox.add(new JLabel(QQ_MATRIX_DESCRIPTION));
		qqMatrix = new DispQQPlotMatrix();
		tableBox.add(qqMatrix);
	}

	/**
	 * Sets up univariate statistics panel
	 */
	private void createUnivariate() {
		Box mainBox = Box.createVerticalBox();
		Box centralBox = Box.createHorizontalBox();
		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(centralBox);
		mainBox.add(Box.createVerticalStrut(10));

		uniStatsPanel.add(mainBox);

		// Pannello dei componenti univariate statistics panel
		JPanel componentsPanel = new JPanel(new BorderLayout(0, 5));

		// Aggiuna label descrizione
		componentsPanel.add(new JLabel(UNIV_DESCRITPION), BorderLayout.NORTH);
		componentsPanel.add(transfGraphCreate(), BorderLayout.SOUTH);
		componentsPanel.add(getScrollPaneTable(), BorderLayout.CENTER);

		// Aggiuna pannello dei componenti al tabbed pane univariate
		centralBox.add(componentsPanel);
	}

	/**
	 * Sets up bivariate statistics panel
	 */
	private void createBivariate() {
		Box mainBox = Box.createVerticalBox();
		Box centralBox = Box.createVerticalBox();
		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(centralBox);
		mainBox.add(Box.createVerticalStrut(10));

		bivStatsPanel.add(mainBox);
		JPanel mainPanel = new JPanel(new BorderLayout(0, 20));
		centralBox.add(mainPanel);
		mainPanel.add(new JLabel(BIVARIATE_DESCRIPTION), BorderLayout.NORTH);
		tableBivariate = new JWatBivariateStatsTable();
		modelBivariate = new JWatBivariateStatsTableModel(model.getMatrix());
		tableBivariate.setModel(modelBivariate);
		panelBiv = new ScrollBivariatePanel(tableBivariate);
		mainPanel.add(panelBiv, BorderLayout.CENTER);
	}

	//UPDATE DB 20/10/2006
	protected AbstractAction EXECUTE_SAMPLING = new AbstractAction("Do Sampling") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			if (session.getListOfClustering().size() > 0) {
				if (JOptionPane.showConfirmDialog(StatsPanel.this,
						"If you apply this sampling all clustering will be deleted. Do you want to continue?", "Warning", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
					session.removeAllClustering();
				} else {
					return;
				}
			}
			int varSel = varsList.getSelectedIndex();
			int sampSel = samplingList.getSelectedIndex();
			switch (sampSel) {
				/** TRIMMING **/
				case 0:
					isSampled = true;
					int percentile = ((Integer) quantile.getValue()).intValue();
					if (!between.isSelected()) {
						model.doSamplingOnVariable(varSel, new TrimmingFilter(
								model.getMatrix().getVariables()[varSel].getUniStats().getQuantili()[percentile - 1], up.isSelected()));
					} else {
						model.doSamplingOnVariable(varSel,
								new TrimmingBetweenFilter(model.getMatrix().getVariables()[varSel].getUniStats().getQuantili()[percentile - 1],
										model.getMatrix().getVariables()[varSel].getUniStats().getQuantili()[((Integer) quantileB.getValue())
												.intValue() - 1]));
					}
					undoSam.setEnabled(true);
					break;
				case 1:
					isSampled = true;
					int nObs = ((Integer) random.getValue()).intValue();
					model.doSamplingOnVariable(0, new RandomFilter(model.getMatrix().getVariables()[0].Size(), nObs));
					undoSam.setEnabled(true);
					break;
				case 2:
					isSampled = true;
					int min = ((Integer) fromO.getValue()).intValue();
					int max = ((Integer) toO.getValue()).intValue();
					model.doSamplingOnVariable(0, new IntervalFilter(min, max));
					undoSam.setEnabled(true);
					break;
				case 3:
					isSampled = true;
					switch (model.getMatrix().getVariables()[varsList.getSelectedIndex()].getType()) {
						case STRING:
							model.doSamplingOnVariable(varsList.getSelectedIndex(), new FilterOnString(varsList.getSelectedIndex(),
									((VariableString) model.getMatrix().getVariables()[varsList.getSelectedIndex()])
											.getListOfMatching(subs.getText())));
							undoSam.setEnabled(true);
							break;
						case NUMERIC:
							model.doSamplingOnVariable(varsList.getSelectedIndex(), new FilterOnNumeric(((Double) minN.getValue()).doubleValue(),
									((Double) maxN.getValue()).doubleValue(), varsList.getSelectedIndex()));
							undoSam.setEnabled(true);
							break;
						case DATE:
							model.doSamplingOnVariable(varsList.getSelectedIndex(), new FilterOnData(((Date) fromD.getValue()), ((Date) toD
									.getValue()), varsList.getSelectedIndex()));
							undoSam.setEnabled(true);
							break;
					}
					break;
			}
		}
	};
	protected AbstractAction UNDO_SAMPLING = new AbstractAction("Undo Sampling") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			if (session.getListOfClustering().size() >= 0) {
				if (JOptionPane.showConfirmDialog(StatsPanel.this.getParentWizard(), "This operation will reset all clusterings done. Continue ?",
						"WARNING", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
					model.undoSamplingOnVariable(varsList.getSelectedIndex());
					session.removeAllClustering();
					undoSam.setEnabled(false);
					isSampled = false;
				} else {
					return;
				}
			}
		}
	};

	private JList varsList = null;
	private JList samplingList = null;
	private JPanel varInfo = new JPanel(new FlowLayout(FlowLayout.LEFT));
	private JLabel varLabel = new JLabel();
	private JPanel optpanel = new JPanel(new BorderLayout());
	private JPanel options = new JPanel();
	private String[] samplingNames = new String[] { "Trimming", "Random", "Observ. # interval", "Interval" };
	private JButton executeSam = new JButton(EXECUTE_SAMPLING);
	private JButton undoSam = new JButton(UNDO_SAMPLING);
	private JPanel south;

	private JList getVariableList() {
		if (varsList == null) {
			varsList = new JList();

			varsList.setSelectionBackground(new Color(83, 126, 126));
			varsList.setSelectionForeground(Color.BLACK);

			varsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			Font f = varsList.getFont();
			//varsList.setPreferredSize(new Dimension(160, 400));
			varsList.setMaximumSize(new Dimension(160, 400));
			varsList.setFont(new Font(f.getFontName(), f.getStyle(), f.getSize() + 2));
			varsList.setBorder(BorderFactory.createLoweredBevelBorder());
			varsList.addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (!e.getValueIsAdjusting() && model.getMatrix() != null) {
						int select = varsList.getSelectedIndex();
						if (select >= 0) {
							switch (model.getMatrix().getVariables()[select].getType()) {
								case STRING:
									varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a String variable and it has been transformed has "
											+ model.getMatrix().getVariables()[select].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
									varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
											.getVariableNames()[select]
											+ " information")));
									break;
								case NUMERIC:
									varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a Numeric variable and it has been transformed has "
											+ model.getMatrix().getVariables()[select].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
									varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
											.getVariableNames()[select]
											+ " information")));
									break;
								case DATE:
									varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a Date variable and it has been transformed has "
											+ model.getMatrix().getVariables()[select].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
									varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
											.getVariableNames()[select]
											+ " information")));
									break;
								default:
							}
							//Abilitazione pulsanti di sampling e undo
							executeSam.setEnabled(true);
							//if(model.getMatrix().getVariables()[select].isSampled())
							if (isSampled) {
								undoSam.setEnabled(true);
							} else {
								undoSam.setEnabled(false);
							}
							//Controllo su sampling selezionato
							if (samplingList.getSelectedIndex() == 3) {
								samplingList.clearSelection();
								samplingList.setSelectedIndex(3);
							}
						} else {
							varsList.setSelectedIndex(0);
							/*varLabel.setText(HTML_START + HTML_FONT_NORM +
									"No variable selected"
									+ HTML_FONT_NOR_END + HTML_END);
							varInfo.setBorder(new TitledBorder(new TitledBorder(
									new EtchedBorder(EtchedBorder.LOWERED), "No variable selected")));
							//DisAbilitazione pulsanti di sampling e undo
							executeSam.setEnabled(false);
							undoSam.setEnabled(false);
							//Controllo su sampling selezionato
							if(samplingList.getSelectedIndex() == 3){
								samplingList.clearSelection();
								samplingList.setSelectedIndex(3);
							}*/
						}
					}
				}
			});
		}
		return varsList;
	}

	private JList getSamplingList() {
		if (samplingList == null) {
			samplingList = new JList(samplingNames);

			samplingList.setSelectionBackground(new Color(83, 126, 126));
			samplingList.setSelectionForeground(Color.BLACK);

			samplingList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			//samplingList.setPreferredSize(new Dimension(160, 400));
			Font f = samplingList.getFont();
			samplingList.setFont(new Font(f.getFontName(), f.getStyle(), f.getSize() + 2));
			samplingList.setBorder(BorderFactory.createLoweredBevelBorder());
			samplingList.addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (!e.getValueIsAdjusting() && model.getMatrix() != null) {
						int index = ((JList) e.getSource()).getSelectedIndex();
						switch (index) {
							case 0:
								sampleDescr.setText(SAMPLING_TRIMMING_DESCRITPION);
								optpanel.remove(options);
								south.setVisible(true);
								options = getTrimmingPanel();
								optpanel.add(options, BorderLayout.CENTER);
								optpanel.revalidate();
								optpanel.repaint();
								varsList.setEnabled(true);
								break;
							case 1:
								sampleDescr.setText(SAMPLING_RANDOM_DESCRITPION);
								optpanel.remove(options);
								south.setVisible(true);
								options = getRandomPanelOption();
								optpanel.add(options, BorderLayout.CENTER);
								optpanel.revalidate();
								optpanel.repaint();
								varsList.setEnabled(false);
								break;
							case 2:
								sampleDescr.setText(SAMPLING_INTERVAL_DESCRITPION);
								optpanel.remove(options);
								south.setVisible(true);
								options = getIntervalPanelOption();
								optpanel.add(options, BorderLayout.CENTER);
								optpanel.revalidate();
								optpanel.repaint();
								varsList.setEnabled(false);
								break;
							case 3:
								sampleDescr.setText(SAMPLING_FILTERING_DESCRITPION);
								optpanel.remove(options);
								south.setVisible(true);
								options = getFilteringPanelOption();
								optpanel.add(options, BorderLayout.CENTER);
								optpanel.revalidate();
								optpanel.repaint();
								varsList.setEnabled(true);
								break;
							default:
								samplingList.setSelectedIndex(0);
								/*							sampleDescr.setText("");
														optpanel.remove(options);
														south.setVisible(false);
														options = new JPanel();
														optpanel.add(options,BorderLayout.CENTER);
														optpanel.revalidate();
														optpanel.repaint();*/
						}
					}
				}
			});
		}
		return samplingList;
	}

	/**
	 * Sets up sampling panel
	 */
	private void createSampling() {
		Box mainbox = Box.createVerticalBox();
		mainbox.add(Box.createVerticalStrut(20));

		Box paneldesclabel = Box.createHorizontalBox();
		Box centralpanel = Box.createHorizontalBox();
		Box warningpanl = Box.createHorizontalBox();

		mainbox.add(paneldesclabel);
		mainbox.add(Box.createVerticalStrut(15));
		mainbox.add(centralpanel);
		mainbox.add(Box.createVerticalStrut(15));
		mainbox.add(warningpanl);
		mainbox.add(Box.createVerticalStrut(15));
		/** Listener on trasformation per change label **/
		model.addOnChangeVariableValue(new ChangeVariableListener() {
			public void onChangeVariableValues() {
				if (varsList.getSelectedIndex() >= 0) {
					switch (model.getMatrix().getVariables()[varsList.getSelectedIndex()].getType()) {
						case STRING:
							varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a String variable and it has been transformed has "
									+ model.getMatrix().getVariables()[varsList.getSelectedIndex()].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
							varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
									.getVariableNames()[varsList.getSelectedIndex()]
									+ " information")));
							break;
						case NUMERIC:
							varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a Numeric variable and it has been transformed has "
									+ model.getMatrix().getVariables()[varsList.getSelectedIndex()].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
							varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
									.getVariableNames()[varsList.getSelectedIndex()]
									+ " information")));
							break;
						case DATE:
							varLabel.setText(HTML_START + HTML_FONT_NORM + "This is a Date variable and it has been transformed has "
									+ model.getMatrix().getVariables()[varsList.getSelectedIndex()].getTrasfStr() + HTML_FONT_NOR_END + HTML_END);
							varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), model.getMatrix()
									.getVariableNames()[varsList.getSelectedIndex()]
									+ " information")));
							break;
						default:
					}
				} else {
					varLabel.setText(HTML_START + HTML_FONT_NORM + "No variable selected" + HTML_FONT_NOR_END + HTML_END);
					varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "No variable selected")));
				}
			}
		});
		/** DESCRIPTION LABEL OF PANEL **/
		JPanel jpdesclabel = new JPanel(new GridLayout(1, 1));
		jpdesclabel.add(new JLabel(SAMPLING_DESCRITPION));
		paneldesclabel.add(jpdesclabel);
		/** CENTRAL PANEL **/
		JPanel jcentralp = new JPanel(new BorderLayout(10, 0));

		optionSampling = new JPanel(new BorderLayout());
		/** VARIABLE INFORMATION PANEL **/
		varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "No variable selected")));
		varInfo.add(varLabel);
		varLabel.setText(HTML_START + HTML_FONT_NORM + "No variable selected" + HTML_FONT_NOR_END + HTML_END);
		varInfo.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "No variable selected")));
		/** SAMPLING OPTION PANEL **/
		optpanel.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Sampling options")));
		sampleDescr = new JLabel();
		optpanel.add(sampleDescr, BorderLayout.EAST);
		optpanel.add(options, BorderLayout.CENTER);
		south = new JPanel(new FlowLayout());
		executeSam.setPreferredSize(new Dimension((int) (BUTTONSIZE * 3.5), (int) (BUTTONSIZE * 0.8)));
		undoSam.setPreferredSize(new Dimension((int) (BUTTONSIZE * 3.5), (int) (BUTTONSIZE * 0.8)));
		south.add(executeSam);
		south.add(undoSam);
		executeSam.setEnabled(false);
		undoSam.setEnabled(false);
		south.setVisible(false);
		optpanel.add(south, BorderLayout.SOUTH);

		optionSampling.add(varInfo, BorderLayout.NORTH);
		optionSampling.add(optpanel, BorderLayout.CENTER);
		/** LISTS **/
		JPanel jlistpanel = new JPanel(new GridLayout(1, 2, 10, 5));
		JPanel varpanel = new JPanel(new BorderLayout());
		JPanel samppanel = new JPanel(new BorderLayout());

		varpanel.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Variables" + HTML_FONT_TIT_END + HTML_END), BorderLayout.NORTH);
		varpanel.add(new JScrollPane(getVariableList(), JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED),
				BorderLayout.CENTER);
		samppanel.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Sampling methods" + HTML_FONT_TIT_END + HTML_END), BorderLayout.NORTH);
		samppanel.add(new JScrollPane(getSamplingList(), JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED),
				BorderLayout.CENTER);

		//jlistpanel.add(varpanel);
		jlistpanel.add(samppanel);
		jlistpanel.add(varpanel);

		jcentralp.add(jlistpanel, BorderLayout.WEST);
		jcentralp.add(optionSampling, BorderLayout.CENTER);
		centralpanel.add(Box.createHorizontalStrut(10));
		centralpanel.add(jcentralp);
		centralpanel.add(Box.createHorizontalStrut(10));
		/** WARNING LABEL **/
		JPanel jpwarning = new JPanel(new GridLayout(1, 1));
		jpwarning.add(new JLabel(SAMPLING_TRIMMING_PANEL_DESC));
		warningpanl.add(jpwarning);

		samplePanel.add(mainbox);
		mainbox.add(Box.createVerticalStrut(20));
	}

	/**
	 * Sets up dispersion matrix panel
	 */
	private void createMatrix() {
		Box mainBox = Box.createVerticalBox();
		Box descBox = Box.createHorizontalBox();
		Box tableBox = Box.createHorizontalBox();

		scatterMatrixPanel.add(mainBox);

		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(descBox);
		mainBox.add(Box.createVerticalStrut(10));
		mainBox.add(tableBox);
		mainBox.add(Box.createVerticalStrut(10));

		descBox.add(new JLabel(MATRIX_DESCRIPTION));
		dispPanel = new DispMatrix();
		tableBox.add(dispPanel);
	}

	/** ************ Univariate panel ************************* */
	private JWatUnivariateStatsTable variableTable = null;
	private JWatUnivariateStatsTableModel variableModel = null;
	private JScrollPane scrollTablePane = null;
	private JComboBox transfs;
	private JPanel transfPanel;
	private JPanel mainTransf;
	private JPanel blankPanel;
	private JLabel transfList;

	private QQPlotPreviewPanel graph1 = null;

	/**
	 * Create univariate panel
	 * 
	 * @return
	 */
	private JPanel transfGraphCreate() {
		JPanel totalPanel = new JPanel(new GridLayout(1, 2));
		mainTransf = new JPanel(new GridLayout(1, 1));
		transfPanel = new JPanel(new BorderLayout());
		help.addHelp(transfPanel, TRANSF_PANEL);
		JPanel graphPanel = new JPanel(new GridLayout(1, 1));
		/** ************** Transformation Panel ********************* */
		transfPanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), TRANSF_BORDER_TEXT + "none"));
		createComboTransf();
		JPanel northPanel = new JPanel(new GridLayout(2, 2, 5, 50));
		JLabel tr = new JLabel("Available transformations:");
		tr.setHorizontalAlignment(JLabel.RIGHT);
		northPanel.add(tr);
		northPanel.add(transfs);
		transfPanel.add(northPanel, BorderLayout.NORTH);
		// JPanel centerTransf = new JPanel(new GridLayout(1,2,5,0));
		northPanel.add(new JButton(applyTranformation));
		northPanel.add(new JButton(undoTranformation));
		transfList = new JLabel(TRANSF_LABEL_APPLIED + "none = none ");
		transfPanel.add(transfList, BorderLayout.CENTER);
		/** ******************* Graph Panel ************************** */
		// TODO: aggiungere i grafici corretti che si zoommano e allargano
		graphPanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Graphs"));
		JTabbedPane graphsTabbed = new JTabbedPane();
		JPanel graph = new JPanel();
		plo = new SmallPlotDistGraph(model);
		plo.setSize(new Dimension(330, 120));
		graph.add(plo);
		graphsTabbed.addTab("Frequencies", graph);
		graph1 = new QQPlotPreviewPanel(model);
		graph1.setSize(new Dimension(330, 120));
		graphsTabbed.addTab("QQ-Plot", graph1);
		graphPanel.add(graphsTabbed);
		//Blank panel
		blankPanel = new JPanel(new BorderLayout());
		blankPanel.add(new JLabel(HTML_START + HTML_FONT_NORM + " This variable could not be transformed since it is not numeric ."
				+ HTML_FONT_NOR_END + HTML_END));
		blankPanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), ""));
		// Add to main panel transformation and graph
		mainTransf.add(transfPanel);
		totalPanel.add(mainTransf);

		totalPanel.add(graphPanel);
		return totalPanel;
	}

	private void createComboTransf() {
		transfs = new JComboBox();
		transfs.setToolTipText(TRANSF_COMBO);
		transfs.addItem("Logarithmic");
		transfs.addItem("Mix - Max");
		transfs.addItem("z-score (Standard Deviation)");
	}

	private JScrollPane getScrollPaneTable() {
		if (scrollTablePane == null) {
			createTable();
			scrollTablePane = new JScrollPane(variableTable, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		}
		return scrollTablePane;
	}

	private JWatUnivariateStatsTable createTable() {
		if (variableTable == null) {
			variableTable = new JWatUnivariateStatsTable(model);
			variableTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			variableTable.setToolTipText(STATS_TABLE);
			help.addHelp(variableTable, STATS_TABLE);
			variableTable.setPlotFreq(plo);
			variableTable.setPlotQQPlot(graph1);
			variableTable.setLabel(transfList);
			variableTable.setPanel(transfPanel);
			variableTable.setShowGrid(true);
			variableTable.setModel(getModel());
			variableTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (variableTable.getSelectedRow() >= 0) {
						mainTransf.removeAll();
						switch (model.getMatrix().getVariables()[variableTable.getSelectedRow()].getType()) {
							case VariableNumber.NUMERIC:
								mainTransf.add(transfPanel);
								break;
							case VariableNumber.DATE:
								blankPanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), TRANSF_BORDER_TEXT
										+ model.getMatrix().getVariables()[(variableTable.getSelectedRow())].getName() + " - Type: Date"));
								mainTransf.add(blankPanel);
								break;
							case VariableNumber.STRING:
								blankPanel.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), TRANSF_BORDER_TEXT
										+ model.getMatrix().getVariables()[(variableTable.getSelectedRow())].getName() + " - Type: String"));
								mainTransf.add(blankPanel);
								break;
						}
						mainTransf.revalidate();
						mainTransf.repaint();
					} else {
						if (model.getMatrix() != null) {
							variableTable.setRowSelectionInterval(0, 0);
						}
					}
				}
			});
		}
		return variableTable;
	}

	private JWatUnivariateStatsTableModel getModel() {
		if (variableModel == null) {
			variableModel = new JWatUnivariateStatsTableModel(model.getMatrix());
		}
		return variableModel;
	}

	/** ************Pannelli per univariate FINE***************************** */
	public String getName() {
		return "Statistics";
	}

	public void gotFocus() {
		((JWatWizard) getParentWizard()).setEnableButton("Solve", false);
		parent.setCurrentPanel(WORKLOAD_INPUT_PANEL);
	}

	private SmallPlotDistGraph plo;

	private DispMatrix dispPanel;

	// Update 19-20/10/2006
	private JPanel TrimmingPanelOption = null;
	private JPanel RandomPanelOption = null;
	private JPanel IntervalPanelOption = null;
	private JPanel FilteringPanelOption = null;
	private JSpinner quantile = null;
	private JSpinner quantileB = null;
	private JSpinner random = null;
	private JSpinner fromO = null;
	private JSpinner toO = null;
	private JRadioButton up = null;
	private JRadioButton down = null;
	private JRadioButton between = null;

	private JPanel getTrimmingPanel() {
		if (TrimmingPanelOption == null) {
			TrimmingPanelOption = new JPanel(new BorderLayout());

			JPanel center = new JPanel(new BorderLayout());

			JPanel centerP = new JPanel(new BorderLayout());

			JPanel centerCenter = new JPanel(new FlowLayout(FlowLayout.LEFT));
			centerCenter.add(new JLabel("        Percentile: "));
			quantile = new JSpinner(new SpinnerNumberModel(5, 1, 99, 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Integer) quantile.getValue()).intValue() >= ((Integer) quantileB.getValue()).intValue()) {
						quantileB.setValue(new Integer(((Integer) quantile.getValue()).intValue() + 1));
					}
				}
			};
			quantile.setPreferredSize(new Dimension(60, 20));
			centerCenter.add(quantile);

			//UPDATE 10/11/06
			between = new JRadioButton("Perc.< x < Perc.");
			between.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent arg0) {
					if (between.isSelected()) {
						quantileB.setEnabled(true);
					} else {
						quantileB.setEnabled(false);
					}
				}
			});
			JPanel c = new JPanel(new FlowLayout(FlowLayout.LEFT));
			quantileB = new JSpinner(new SpinnerNumberModel(5, 2, 99, 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Integer) quantileB.getValue()).intValue() <= ((Integer) quantile.getValue()).intValue()) {
						quantile.setValue(new Integer(((Integer) quantileB.getValue()).intValue() - 1));
					}
				}
			};
			quantileB.setPreferredSize(new Dimension(60, 20));
			c.add(new JLabel("        Percentile: "));
			c.add(quantileB);
			quantileB.setEnabled(false);
			JPanel p = new JPanel(new GridLayout(2, 1));
			p.add(centerCenter);
			p.add(c);
			centerP.add(p, BorderLayout.CENTER);
			//UPDATE 10/11/06

			//centerP.add(centerCenter,BorderLayout.CENTER);
			ButtonGroup opt = new ButtonGroup();
			up = new JRadioButton(" x > Percentile");
			down = new JRadioButton(" x < Percentile");

			down.setSelected(true);
			opt.add(down);
			opt.add(up);
			opt.add(between);
			JPanel optionP = new JPanel(new GridLayout(3, 1));
			optionP.add(down);
			optionP.add(up);
			optionP.add(between);

			centerP.add(optionP, BorderLayout.SOUTH);

			center.add(centerP, BorderLayout.NORTH);

			TrimmingPanelOption.add(center, BorderLayout.CENTER);
		}
		if (varsList.getSelectedIndex() == -1) {
			executeSam.setEnabled(false);
			undoSam.setEnabled(false);
		}
		return TrimmingPanelOption;
	}

	private JPanel getRandomPanelOption() {
		if (RandomPanelOption == null) {
			RandomPanelOption = new JPanel(new BorderLayout());

			JPanel center = new JPanel(new BorderLayout());

			JPanel centerCenter = new JPanel(new FlowLayout(FlowLayout.LEFT));
			centerCenter.add(new JLabel("Number of random obs.: "));
			random = new JSpinner(new SpinnerNumberModel(1, 1, model.getMatrix().getVariables()[0].Size(), 1));
			random.setPreferredSize(new Dimension(60, 25));
			centerCenter.add(random);
			center.add(centerCenter, BorderLayout.CENTER);

			RandomPanelOption.add(center, BorderLayout.CENTER);
		}
		if (varsList.getSelectedIndex() == -1) {
			executeSam.setEnabled(false);
			undoSam.setEnabled(false);
		}
		return RandomPanelOption;
	}

	private JPanel getIntervalPanelOption() {
		if (IntervalPanelOption == null) {
			IntervalPanelOption = new JPanel(new BorderLayout());

			JPanel center = new JPanel(new BorderLayout());

			JPanel centerP = new JPanel(new FlowLayout(FlowLayout.LEFT));
			JLabel lbl = new JLabel("From obs. number: ");
			lbl.setPreferredSize(new Dimension(100, 25));
			centerP.add(lbl);
			fromO = new JSpinner(new SpinnerNumberModel(1, 1, model.getMatrix().getVariables()[0].Size() - 1, 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Integer) fromO.getValue()).intValue() >= ((Integer) toO.getValue()).intValue()) {
						toO.setValue(new Integer(((Integer) fromO.getValue()).intValue() + 1));
					}
				}
			};
			fromO.setPreferredSize(new Dimension(70, 25));
			centerP.add(fromO);
			JLabel lbl2 = new JLabel("To obs. number: ");
			lbl2.setPreferredSize(new Dimension(100, 25));
			centerP.add(lbl2);
			toO = new JSpinner(new SpinnerNumberModel(2, 2, model.getMatrix().getVariables()[0].Size(), 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Integer) toO.getValue()).intValue() <= ((Integer) fromO.getValue()).intValue()) {
						fromO.setValue(new Integer(((Integer) toO.getValue()).intValue() - 1));
					}
				}
			};
			toO.setPreferredSize(new Dimension(70, 25));
			centerP.add(toO);

			center.add(centerP);

			IntervalPanelOption.add(center, BorderLayout.CENTER);
		}
		if (varsList.getSelectedIndex() == -1) {
			executeSam.setEnabled(false);
			undoSam.setEnabled(false);
		}
		return IntervalPanelOption;
	}

	private JPanel filterS, filterD, filterN;
	private JTextField subs;
	private JSpinner minN, maxN;
	//private CalendarComboBox fromD,toD;
	private JFormattedTextField fromD, toD;

	private JPanel getFilteringPanelOption() {
		if (FilteringPanelOption == null) {
			FilteringPanelOption = new JPanel(new BorderLayout());

			//Pannello filtering on String
			filterS = new JPanel(new FlowLayout());
			JLabel l = new JLabel("Substring to be matched");
			subs = new JTextField();
			subs.setPreferredSize(new Dimension(120, 25));
			filterS.add(l);
			filterS.add(subs);

			//Pannello filtering on Data
			filterD = new JPanel(new FlowLayout(FlowLayout.LEFT));
			JLabel lfrom = new JLabel("From date: ");
			lfrom.setPreferredSize(new Dimension(60, 25));
			fromD = new JFormattedTextField(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));
			JLabel lto = new JLabel("To date: ");
			lto.setPreferredSize(new Dimension(60, 25));
			toD = new JFormattedTextField(new SimpleDateFormat("dd.MM.yyyy HH:mm:ss"));
			filterD.add(lfrom);
			filterD.add(fromD);
			filterD.add(lto);
			filterD.add(toD);

			//Pannello di filtering on Numeric
			filterN = new JPanel(new FlowLayout(FlowLayout.LEFT));
			minN = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 0.0, 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Double) minN.getValue()).doubleValue() >= ((Double) maxN.getValue()).doubleValue()) {
						maxN.setValue(new Double(((Double) minN.getValue()).doubleValue() + 1));
					}
				}
			};
			maxN = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 0.0, 1)) {
				public void setValue(Object o) {
					super.setValue(o);
					if (((Double) maxN.getValue()).doubleValue() <= ((Double) minN.getValue()).doubleValue()) {
						minN.setValue(new Double(((Double) maxN.getValue()).doubleValue() - 1));
					}
				}
			};
			JLabel min = new JLabel("Minimum value: ");
			min.setPreferredSize(new Dimension(90, 25));
			minN.setMaximumSize(new Dimension(600, 25));
			filterN.add(min);
			filterN.add(minN);
			JLabel max = new JLabel("Maximum value: ");
			max.setPreferredSize(new Dimension(90, 25));
			maxN.setMaximumSize(new Dimension(600, 25));
			filterN.add(max);
			filterN.add(maxN);

		}
		//In base al tipo di variabile visualizzo il pannello
		if (varsList.getSelectedIndex() == -1) {
			executeSam.setEnabled(false);
			undoSam.setEnabled(false);
			FilteringPanelOption.removeAll();
			FilteringPanelOption.revalidate();
			FilteringPanelOption.repaint();
		} else {
			FilteringPanelOption.removeAll();
			switch (model.getMatrix().getVariables()[varsList.getSelectedIndex()].getType()) {
				case STRING:
					FilteringPanelOption.add(filterS, BorderLayout.CENTER);
					break;
				case DATE:
					FilteringPanelOption.add(filterD, BorderLayout.CENTER);
					fromD.setValue(new Date((long) model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMinValue()));
					toD.setValue(new Date((long) model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMaxValue()));
					break;
				case NUMERIC:
					FilteringPanelOption.add(filterN, BorderLayout.CENTER);
					minN.setModel(new SpinnerNumberModel(model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMinValue(),
							model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMinValue(), model.getMatrix()
									.getVariables()[varsList.getSelectedIndex()].getUniStats().getMaxValue(), 1));
					maxN.setModel(new SpinnerNumberModel(model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMaxValue(),
							model.getMatrix().getVariables()[varsList.getSelectedIndex()].getUniStats().getMinValue(), model.getMatrix()
									.getVariables()[varsList.getSelectedIndex()].getUniStats().getMaxValue(), 1));
					break;
			}
			FilteringPanelOption.revalidate();
			FilteringPanelOption.repaint();
		}
		return FilteringPanelOption;
	}

	private static final String helpText = "<HTML>"
			+ " This panel shows univariate and bivariate statistics, allow numeric variable transformations and<p>sampling operations.<br>"
			+ "<UL><LI>In Univariate panel select a variable in table, apply transformations and see QQ-Plot or<p>frequencies graphs.</LI>"
			+ "<LI>In Sampling panel select a sampling method and apply it (interval, trimming and random).</LI>"
			+ "<LI>Bivariate panel shows correlation coefficient.</LI>"
			+ "<LI>QQ-plot and scatter matrix panels shows every variable vs. variable graphs and can be<p>enlarged with double click.</LI></html>";

	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);
	}

	public void lostFocus() {
		parent.setLastPanel(WORKLOAD_BIVARIATE_PANEL);
	}

}
