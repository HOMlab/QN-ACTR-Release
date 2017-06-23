package jmt.gui.jwat.workloadAnalysis.tables;

import java.awt.Color;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;

import jmt.engine.jwat.workloadAnalysis.utils.ChangeVariableListener;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.workloadAnalysis.chart.QQPlotPreviewPanel;
import jmt.gui.jwat.workloadAnalysis.chart.SmallPlotDistGraph;
import jmt.gui.jwat.workloadAnalysis.panels.StatsPanel;

public class JWatUnivariateStatsTable extends JTable implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	// Size of each columns
	protected int[] columnSizes = new int[] { 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80 };
	// Workload model
	private ModelWorkloadAnalysis model;
	private SmallPlotDistGraph graphF = null;
	private QQPlotPreviewPanel graphQQ = null;
	private JPanel labelPanel = null;
	private JLabel label = null;

	// Contructor
	public JWatUnivariateStatsTable(ModelWorkloadAnalysis model) {
		setSelectionBackground(new Color(83, 126, 126));
		setSelectionForeground(Color.BLACK);

		this.setRowSelectionAllowed(true);
		this.setColumnSelectionAllowed(false);
		this.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					if (JWatUnivariateStatsTable.this.getSelectedRow() >= 0) {
						if (graphF != null) {
							graphF.draw(JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
									.getSelectedRow())], (JWatUnivariateStatsTable.this.getSelectedRow()));
							graphQQ.setCurrentVar((JWatUnivariateStatsTable.this.getSelectedRow()));
						}
						if (label != null) {
							if (JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this.getSelectedRow())]
									.getTrasfStr().length() > 0) {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName()
										+ " = "
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getTrasfStr());
							} else {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName() + " = none");
							}
						}
						if (labelPanel != null) {
							String ty = null;
							switch (JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this.getSelectedRow())]
									.getType()) {
								case JWATConstants.NUMERIC:
									ty = "Numeric";
									break;
								case JWATConstants.DATE:
									ty = "Date";
									break;
								case JWATConstants.STRING:
									ty = "String";
									break;
							}
							((TitledBorder) labelPanel.getBorder())
									.setTitle(StatsPanel.TRANSF_BORDER_TEXT
											+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
													.getSelectedRow())].getName() + " - Type: " + ty);
							labelPanel.repaint();
						}
					}
				}
			}
		});
		if (this.model != null) {
			this.model.addOnChangeVariableValue(new ChangeVariableListener() {
				public void onChangeVariableValues() {
					if (JWatUnivariateStatsTable.this.getSelectedRow() != -1) {
						if (graphF != null) {
							graphF.draw(JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
									.getSelectedRow())], (JWatUnivariateStatsTable.this.getSelectedRow()));
							graphQQ.setCurrentVar((JWatUnivariateStatsTable.this.getSelectedRow()));
						}
						if (label != null) {
							if (JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this.getSelectedRow())]
									.getTrasfStr().length() > 0) {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName()
										+ " = "
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getTrasfStr());
							} else {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName() + " = none");
							}
						}
						JWatUnivariateStatsTable.this.tableChanged(new TableModelEvent(JWatUnivariateStatsTable.this.getModel()));
					}
				}
			});
		}
		this.model = model;
	}

	// Sets a table model for visualization and editing of data
	public void setModel(JWatUnivariateStatsTableModel tabMod) {
		super.setModel(tabMod);
		setRowHeight(ROW_HEIGHT);
		for (int i = 0; i < columnSizes.length; i++) {
			this.getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
		}
		this.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		if (this.model != null) {
			this.model.addOnChangeVariableValue(new ChangeVariableListener() {
				public void onChangeVariableValues() {
					if (JWatUnivariateStatsTable.this.getSelectedRow() != -1) {
						if (graphF != null) {
							graphF.draw(JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
									.getSelectedRow())], (JWatUnivariateStatsTable.this.getSelectedRow()));
							JWatUnivariateStatsTable.this.setRowSelectionInterval(0, 0);
							graphQQ.setCurrentVar((JWatUnivariateStatsTable.this.getSelectedRow()));
						}
						if (label != null) {
							if (JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this.getSelectedRow())]
									.getTrasfStr().length() > 0) {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName()
										+ " = "
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getTrasfStr());
							} else {
								label.setText(StatsPanel.TRANSF_LABEL_APPLIED
										+ JWatUnivariateStatsTable.this.model.getMatrix().getVariables()[(JWatUnivariateStatsTable.this
												.getSelectedRow())].getName() + " = none");
							}
						}
						JWatUnivariateStatsTable.this.tableChanged(new TableModelEvent(JWatUnivariateStatsTable.this.getModel()));
					}
				}
			});
		}
	}

	public void setPlotFreq(SmallPlotDistGraph g) {
		graphF = g;
	}

	public void setPlotQQPlot(QQPlotPreviewPanel g) {
		graphQQ = g;
	}

	public void setLabel(JLabel lab) {
		label = lab;
	}

	public void setPanel(JPanel p) {
		labelPanel = p;
	}
}