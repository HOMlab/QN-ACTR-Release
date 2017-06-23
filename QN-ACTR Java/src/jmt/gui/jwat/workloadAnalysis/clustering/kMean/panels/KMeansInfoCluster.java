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
package jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;

import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusteringInfosKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusterInfoKMean.SCluStat;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.ColumnHeaderToolTips;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels.SingleKMeanClusterInfo;

//UPDATE 02/11/2006: + Creazione classe

public class KMeansInfoCluster extends JPanel implements CommonConstants, JWATConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ClusteringInfosKMean infos = null;
	private int numClusters = 0;
	private ModelWorkloadAnalysis model = null;
	private WorkloadAnalysisSession session = null;

	private static final String PANEL_TITLE = HTML_START + HTML_FONT_TITLE + "Cluster Information" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "This panel shows information of variables (center and statistics) within a single cluster" + HTML_FONT_NOR_END + HTML_END;
	private static final String DESCR_LABEL = HTML_START + HTML_FONT_NORM + "Cluster X/Y has Z observations" + HTML_FONT_NOR_END + HTML_END;

	private JList clustList = null;
	private JTable clustInfo = null;

	private SingleKMeanClusterInfo clustDet;

	private JLabel desctText = null;

	private int clustering;

	public KMeansInfoCluster(ClusteringInfosKMean info, int i, WorkloadAnalysisSession m, int clustering) {
		model = (ModelWorkloadAnalysis) m.getDataModel();
		session = m;
		infos = info;
		numClusters = i;
		this.clustering = clustering;
		//initPanel();
		initPanel2();
	}

	private void initPanel2() {
		Box mainPanel = Box.createVerticalBox();
		Box descLabelP = Box.createHorizontalBox();
		Box clusterP = Box.createHorizontalBox();
		Box centralP = Box.createVerticalBox();

		mainPanel.add(Box.createHorizontalStrut(10));
		mainPanel.add(centralP);
		mainPanel.add(Box.createHorizontalStrut(10));

		centralP.add(Box.createVerticalStrut(10));
		centralP.add(descLabelP);
		centralP.add(Box.createVerticalStrut(10));
		centralP.add(clusterP);
		centralP.add(Box.createVerticalStrut(10));

		setLayout(new GridLayout(2, 1));
		add(mainPanel);
		clustDet = new SingleKMeanClusterInfo(session, clustering, numClusters);
		add(clustDet);

		//Stringa informativa
		JPanel p = new JPanel(new GridLayout(1, 1));
		p.add(new JLabel(PANEL_TITLE));
		descLabelP.add(p, BorderLayout.NORTH);

		//Pannello laterale con JList Clusters
		JPanel p1 = new JPanel(new BorderLayout(5, 5));
		JPanel temp = new JPanel(new FlowLayout());
		temp.add(getClusterList());
		p1.add(temp, BorderLayout.WEST);

		//Pannello Con JTable
		JPanel p2 = new JPanel(new BorderLayout(5, 10));
		desctText = new JLabel(DESCR_LABEL);
		p2.add(desctText, BorderLayout.NORTH);
		p2.add(getTable(), BorderLayout.CENTER);
		String cur = DESCR_LABEL.replaceFirst("X", Integer.toString(clustList.getSelectedIndex() + 1));
		cur = cur.replaceFirst("Y", Integer.toString(numClusters));
		desctText.setText(cur.replaceFirst("Z", Integer.toString(infos.numElem[clustList.getSelectedIndex()])));
		p1.add(p2, BorderLayout.CENTER);
		clusterP.add(p1);
	}

	private JScrollPane getTable() {
		if (clustInfo == null) {
			clustInfo = new JTable(new clustTableModel(infos.infoCluster[clustList.getSelectedIndex()].statClust)) {
				/**
				 * 
				 */
				private static final long serialVersionUID = 1L;
				{
					setAutoResizeMode(AUTO_RESIZE_OFF);
					getColumnModel().getColumn(1).setPreferredWidth(130);
					getColumnModel().getColumn(2).setPreferredWidth(75);
					getColumnModel().getColumn(0).setPreferredWidth(30);
					ColumnHeaderToolTips tips = new ColumnHeaderToolTips();
					tips.setToolTip(getColumnModel().getColumn(0), "Considered in the clustering");
					getColumnModel().getColumn(3).setPreferredWidth(75);
					getColumnModel().getColumn(4).setPreferredWidth(75);
					getColumnModel().getColumn(5).setPreferredWidth(75);
					getTableHeader().addMouseMotionListener(tips);
				}

				@Override
				public TableCellRenderer getCellRenderer(int row, int column) {
					if (column == 0) {
						return getDefaultRenderer(Boolean.class);
					}
					return getDefaultRenderer(String.class);
				}
			};
			//clustInfo.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
			clustInfo.setSelectionBackground(new Color(83, 126, 126));
			clustInfo.setSelectionForeground(Color.BLACK);
		}

		JScrollPane p = new JScrollPane(clustInfo);
		return p;
	}

	private JScrollPane getClusterList() {
		if (clustList == null) {
			clustList = new JList();

			clustList.setSelectionBackground(new Color(83, 126, 126));
			clustList.setSelectionForeground(Color.BLACK);
			clustList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			clustList.setFont(new Font(clustList.getFont().getName(), clustList.getFont().getStyle(), clustList.getFont().getSize() + 1));

			String[] clustStr = new String[numClusters];
			for (int i = 0; i < numClusters; i++) {
				clustStr[i] = "Cluster " + (i + 1);
			}

			clustList.setListData(clustStr);

			clustList.addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (clustInfo != null && clustList.getSelectedIndex() >= 0) {
						if (!e.getValueIsAdjusting()) {
							((clustTableModel) clustInfo.getModel()).setData(infos.infoCluster[clustList.getSelectedIndex()].statClust);
							clustInfo.tableChanged(new TableModelEvent(clustInfo.getModel()));
							clustDet.setCluster(clustList.getSelectedIndex());
							String cur = DESCR_LABEL.replaceFirst("X", Integer.toString(clustList.getSelectedIndex() + 1));
							cur = cur.replaceFirst("Y", Integer.toString(numClusters));
							desctText.setText(cur.replaceFirst("Z", Integer.toString(infos.numElem[clustList.getSelectedIndex()])));
						}
					}
				}
			});
		}
		clustList.setSelectedIndex(0);

		JScrollPane p = new JScrollPane(clustList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		p.setPreferredSize(new Dimension(120, 115));
		return p;
	}

	private class clustTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] header = { "Sel.", "Name", "Center", "Std. Dev.", "Kurt.", "Skew." };
		private SCluStat[] data = null;

		public clustTableModel(SCluStat[] stats) {
			data = stats;
		}

		public int getRowCount() {
			return data.length;
		}

		public int getColumnCount() {
			return header.length;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < data.length) {
				if (columnIndex == 1) {
					return model.getMatrix().getVariableNames()[rowIndex];
				}
				if (columnIndex == 0) {
					int[] sel = ((KMean) session.getListOfClustering().get(clustering)).getVarClust();
					for (int element : sel) {
						if (element == rowIndex) {
							return Boolean.TRUE;
						}
					}
					return Boolean.FALSE;
				}
				if (columnIndex == 2) {
					return defaultFormat.format(data[rowIndex].dMedia);
				}
				if (columnIndex == 3) {
					return defaultFormat.format(data[rowIndex].dStdDv);
				}
				if (columnIndex == 4) {
					return defaultFormat.format(data[rowIndex].dKurto);
				}
				if (columnIndex == 5) {
					return defaultFormat.format(data[rowIndex].dSkewn);
				}
			}
			return null;
		}

		@Override
		public String getColumnName(int columnIndex) {
			if (columnIndex < header.length) {
				return header[columnIndex];
			}
			return "";
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

		@Override
		public Class getColumnClass(int index) {
			return String.class;
		}

		public void setData(SCluStat[] data) {
			this.data = data;
		}
	}
}
