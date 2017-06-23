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
package jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SpinnerNumberModel;
import javax.swing.table.AbstractTableModel;

import jmt.engine.jwat.DinamicFormat;
import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.ClusteringInfosFuzzy;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.workloadAnalysis.chart.DispFuzzyMatrix;

public class FuzzyInfoClustering extends JPanel implements CommonConstants, JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ClusteringInfosFuzzy infos = null;
	private DispFuzzyMatrix matrix;
	private JTable clusteringFinalTable = null;
	private JLabel infoClustering = null;
	private JSpinner fuzzyError = null;
	private JButton selectErrore = null;
	private JLabel descrSetError = null;
	private int numClust = 0;
	private double entropy = 0.0;
	private MatrixOsservazioni m;
	private FuzzyInfoCluster fPanel = null;

	private String INFO_CLUSTERING = HTML_START + HTML_FONT_TITLE + "CLUSTERING INFORMATION " + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This clustering has been performed searching X clusters and the entropy for this<p>result is Y" + HTML_FONT_NOR_END + HTML_END;
	private String DESCR_ERRROR = HTML_START + HTML_FONT_TITLE + "ERROR SETTING INFORMATION " + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "You can select which error to use to create each<p>single cluster according to Fuzzy K-Means<p>clustering results" + HTML_FONT_NOR_END
			+ HTML_END;

	public FuzzyInfoClustering(MatrixOsservazioni m, DispFuzzyMatrix f, int numC, double ent, ClusteringInfosFuzzy c) {
		matrix = f;
		numClust = numC;
		entropy = ent;
		infos = c;
		this.m = m;
		initPanel();
	}

	public void setPanelCluster(FuzzyInfoCluster f) {
		fPanel = f;
	}

	private void initPanel() {
		this.setLayout(new GridLayout(1, 1));
		Box mainPanel = Box.createHorizontalBox();
		mainPanel.add(Box.createHorizontalStrut(5));
		/** Pannello principale centrale **/
		JPanel centralMainP = new JPanel(new BorderLayout(5, 10));
		/** Upper Panel **/
		JPanel upperP = new JPanel(new BorderLayout(5, 5));
		/** Stringa infos clustering **/
		String current = new String(INFO_CLUSTERING);
		current = current.replaceFirst("X", Integer.toString(numClust));
		current = current.replaceFirst("Y", defaultFormat.format(entropy));
		infoClustering = new JLabel(current);
		upperP.add(infoClustering, BorderLayout.NORTH);
		/** Pannello selezione fuzzy error**/
		JPanel fuzzyErrorP = new JPanel(new GridLayout(1, 2));
		/** Pannello text,spinner,button fuzzy error**/
		JPanel leftFuzzy = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 5));
		JLabel testoSpinner = new JLabel("Select the error: ");
		fuzzyError = new JSpinner(new SpinnerNumberModel(0.1, 0.01, 0.99, 0.01));
		fuzzyError.setValue(new Double(infos.getError()));
		fuzzyError.setPreferredSize(new Dimension(60, 25));
		selectErrore = new JButton("Apply error");
		selectErrore.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				/** Impostazione errore per visualizzazione matrice dispersione **/
				infos.setError(m, ((Double) fuzzyError.getValue()).doubleValue());
				matrix.setFuzzyNess(((Double) fuzzyError.getValue()).doubleValue());
				((clustDetModel) clusteringFinalTable.getModel()).setInfos(infos.numElem, infos.percent);
				fPanel.setInfos();
				/** Impostazioni valori per tabella **/
			}
		});
		leftFuzzy.add(testoSpinner);
		leftFuzzy.add(fuzzyError);
		leftFuzzy.add(selectErrore);
		/** Pannello con testo descrizione error **/
		descrSetError = new JLabel(DESCR_ERRROR);
		fuzzyErrorP.add(leftFuzzy);
		fuzzyErrorP.add(descrSetError);
		upperP.add(fuzzyErrorP, BorderLayout.SOUTH);
		/** Table results **/
		centralMainP.add(getUpperTable(), BorderLayout.CENTER);
		centralMainP
				.add(
						new JLabel(
								"<HTML><b><font size = 4>WARNING</font></b><br><font size=3>The sum can be greater than 100% due to multiple observations assignement<br></font>"),
						BorderLayout.SOUTH);
		centralMainP.add(upperP, BorderLayout.NORTH);
		mainPanel.add(centralMainP);
		mainPanel.add(Box.createHorizontalStrut(5));
		this.add(mainPanel);
	}

	private JScrollPane getUpperTable() {
		clusteringFinalTable = new JTable(new clustDetModel(infos.numElem, infos.percent));
		clusteringFinalTable.setSelectionBackground(new Color(83, 126, 126));
		clusteringFinalTable.setSelectionForeground(Color.BLACK);
		clusteringFinalTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		return new JScrollPane(clusteringFinalTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	}

	private class clustDetModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] colHeader = { "Cluster", "Num. observations", "Percentage" };
		private int[] Elem = null;
		private double[] Perc = null;
		DinamicFormat f = new DinamicFormat("###.###%", 0); //????? CONTROLLARE A COSA E SE SERVE 

		public clustDetModel(int[] el, double perc[]) {
			Elem = el;
			Perc = perc;
		}

		public int getRowCount() {
			return Elem.length;
		}

		public int getColumnCount() {
			return colHeader.length;
		}

		public void setInfos(int[] el, double[] perc) {
			Elem = el;
			Perc = perc;
			fireTableDataChanged();
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex >= 0) {
				if (columnIndex == 0) {
					if (rowIndex == Elem.length - 1) {
						return "Not assigned";
					} else {
						return "Cluster " + (rowIndex + 1);
					}
				}
				if (columnIndex == 1) {
					return Integer.toString(Elem[rowIndex]);
				}
				if (columnIndex == 2) {
					return f.format(Perc[rowIndex]);
				}
			}
			return null;
		}

		/**
		 * Returns name for each column (given its index) to be displayed inside
		 * table header
		 */
		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex < colHeader.length) {
				return colHeader[columnIndex];
			} else {
				return null;
			}
		}

		/**
		 * Tells wether data contained in a specific cell(given row and column
		 * index) is editable or not. In this case distribution column is not
		 * editable, as editing functionality is implemented via edit button
		 */
		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return false;
		}
	}
}
