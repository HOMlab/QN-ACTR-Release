package jmt.gui.jwat.workloadAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.clustering.Clustering;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.ClusteringInfosFuzzy;
import jmt.engine.jwat.workloadAnalysis.clustering.fuzzyKMean.FuzzyKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusteringInfosKMean;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMean;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.MainJwatWizard;
import jmt.gui.jwat.workloadAnalysis.chart.DispFuzzyMatrix;
import jmt.gui.jwat.workloadAnalysis.chart.DispKMeanMatrix;
import jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels.DispersionFuzzyPanel;
import jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels.FuzzyInfoCluster;
import jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels.FuzzyInfoClustering;
import jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels.DispersionkMeanPanel;
import jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels.KMeansInfoCluster;
import jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels.KMeansInfoClustering;

public class ClusteringInfoPanel extends WizardPanel implements CommonConstants, JWATConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final String CLUSTERING_INFO_DESCRIPTION = HTML_START
			+ HTML_FONT_TITLE
			+ "Clustering information"
			+ HTML_FONT_TIT_END
			+ HTML_FONT_NORM
			+ "Select a clustering from Clustering table, cluster partition from the Cluster table and see statistics and graphs of the algorithm result"
			+ HTML_FONT_NOR_END + HTML_END;
	private ModelWorkloadAnalysis model;
	private WorkloadAnalysisSession session;

	private JTable clusteringTable;
	private JTable clusterTable;

	private JTabbedPane tabbed;

	private boolean changedType = false;

	private DispKMeanMatrix matrix;
	private DispFuzzyMatrix matrix2;
	private JPanel clusingP;
	private JPanel clustP;
	private JPanel matrixPanel;
	private JPanel cluster = new JPanel(new BorderLayout(0, 5));

	protected AbstractAction deleteVar = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Delete");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent arg0) {
			if (clusteringTable.getSelectedRow() >= 0) {
				session.removeClustering(clusteringTable.getSelectedRow());
				clusteringTable.tableChanged(new TableModelEvent(clusteringTable.getModel()));
				//Non ci sono più clustering passo a finestra precedente
				if (session.getListOfClustering().size() == 0) {
					parent.setLastPanel(WORKLOAD_CLUSTERING_PANEL);
					parent.setLastPanel();
				}
			}
		}
	};

	protected JButton delVar = new JButton() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			setAction(deleteVar);
			setFocusable(true);
		}
	};

	private MainJwatWizard parent;
	private HoverHelp help = null;

	public ClusteringInfoPanel(MainJwatWizard parent) {
		this.parent = parent;
		this.model = (ModelWorkloadAnalysis) parent.getModel();
		this.session = (WorkloadAnalysisSession) parent.getSession();
		this.help = parent.getHelp();
		initPanel();
	}

	@Override
	public String getName() {
		return "Clustering Information";
	}

	private void initPanel() {
		this.setLayout(new BorderLayout(20, 5));

		/*Set Description Label*/
		Box label = Box.createVerticalBox();
		label.add(Box.createVerticalStrut(10));
		label.add(new JLabel(CLUSTERING_INFO_DESCRIPTION));
		this.add(label, BorderLayout.NORTH);
		//JPanel p1 = new JPanel(new FlowLayout(FlowLayout.LEFT));
		JPanel p1 = new JPanel(new BorderLayout());

		/*Set list of clustering*/
		Box listsPanel = Box.createHorizontalBox();
		listsPanel.add(Box.createHorizontalStrut(10));

		JPanel west = new JPanel(new GridLayout(2, 1, 0, 0));
		west.setPreferredSize(new Dimension(200, 400));

		listsPanel.add(west);
		listsPanel.add(Box.createHorizontalStrut(10));

		JPanel clusting = new JPanel(new BorderLayout(0, 5));
		//JPanel cluster=new JPanel(new BorderLayout(0,5));

		//La Table del clustering sempre presente e poi aggiunta a seconda del tipo
		clusting.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Clusterings" + HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM), BorderLayout.NORTH);
		clusting.add(new JScrollPane(getClusteringTable(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED), BorderLayout.CENTER);

		//cluster.add(new JLabel(HTML_START
		//		+ HTML_FONT_TITLE +"Num. Of Clusters"+ HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM),BorderLayout.NORTH);
		//cluster.add(new JScrollPane(getClusterTable(),JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED),BorderLayout.CENTER);

		west.add(clusting);
		west.add(cluster);
		p1.add(listsPanel, BorderLayout.WEST);

		/*Set Tabbed for statistics*/
		tabbed = new JTabbedPane();
		tabbed.setPreferredSize(new Dimension(600, 400));
		p1.add(tabbed, BorderLayout.CENTER);

		clusingP = new JPanel(new GridLayout(1, 1));
		clusingP.setName("Clustering Info");
		clusingP.setPreferredSize(new Dimension(600, 400));
		tabbed.add(clusingP);

		clustP = new JPanel(new GridLayout(1, 1));
		clustP.setName("Cluster Info");
		clustP.setPreferredSize(new Dimension(600, 400));
		tabbed.add(clustP);

		matrixPanel = new JPanel(new GridLayout(1, 1));
		matrixPanel.setPreferredSize(new Dimension(600, 400));
		matrixPanel.setName("Dispersion Matrix");
		tabbed.add(matrixPanel);

		this.add(p1, BorderLayout.CENTER);
	}

	private JTable getClusteringTable() {
		clusteringTable = new JTable(new ClusteringTableModel(session.getListOfClustering())) {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			{
				getColumnModel().getColumn(0).setPreferredWidth(110);
				getColumnModel().getColumn(1).setPreferredWidth(20);
				getColumnModel().getColumn(2).setPreferredWidth(18);
				setRowHeight(18);
			}

			@Override
			public TableCellRenderer getCellRenderer(int row, int column) {
				if (column == 2) {
					return new ButtonCellEditor(delVar);
				}
				return getDefaultRenderer(String.class);
			}

			@Override
			public TableCellEditor getCellEditor(int row, int column) {
				if (column == 2) {
					return new ButtonCellEditor(new JButton(deleteVar));
				}
				return super.getCellEditor(row, column);
			}
		};
		clusteringTable.setFont(new Font(clusteringTable.getFont().getName(), clusteringTable.getFont().getStyle(), clusteringTable.getFont()
				.getSize() + 1));
		clusteringTable.setSelectionBackground(new Color(83, 126, 126));
		clusteringTable.setSelectionForeground(Color.BLACK);
		clusteringTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		clusteringTable.setBorder(BorderFactory.createLoweredBevelBorder());
		clusteringTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					if (clusteringTable.getSelectedRow() >= 0) {
						changedType = true;
						//Selezionare il tipo do clustering corrente
						switch (session.getListOfClustering().get(clusteringTable.getSelectedRow()).getClusteringType()) {
							case KMEANS:
								resetPanels();
								cluster.removeAll();
								cluster.add(
										new JLabel(HTML_START + HTML_FONT_TITLE + "Num. of clusters" + HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM),
										BorderLayout.NORTH);
								cluster.add(new JScrollPane(getClusterTable(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
										ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED), BorderLayout.CENTER);
								cluster.revalidate();
								cluster.repaint();
								((ClusterTableModelKMeans) clusterTable.getModel()).setCluster(((KMean) session.getListOfClustering().get(
										clusteringTable.getSelectedRow())).getResults());
								tabbed.setSelectedIndex(0);
								break;
							case FUZZYK:
								resetPanels();
								cluster.removeAll();
								cluster.add(
										new JLabel(HTML_START + HTML_FONT_TITLE + "Num. of clusters" + HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM),
										BorderLayout.NORTH);
								cluster.add(new JScrollPane(getClusterTable(), ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
										ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED), BorderLayout.CENTER);
								cluster.revalidate();
								cluster.repaint();
								((ClusterTableModelFuzzy) clusterTable.getModel()).setCluster(((FuzzyKMean) session.getListOfClustering().get(
										clusteringTable.getSelectedRow())).getEntropy());
								tabbed.setSelectedIndex(0);
								break;
						}
						clusterTable.setRowSelectionInterval(0, 0);
					} else {
						if (clusteringTable.getRowCount() > 0) {
							clusteringTable.setRowSelectionInterval(0, 0);
						} else {
							//Annullare pannelli
							resetPanels();
						}
					}
				}
			}
		});
		return clusteringTable;
	}

	private void resetPanels() {
		clusingP.removeAll();
		clustP.removeAll();
		matrixPanel.removeAll();
		matrixPanel.revalidate();
		matrixPanel.repaint();
		clusingP.revalidate();
		clusingP.repaint();
		clustP.revalidate();
		clustP.repaint();
		cluster.removeAll();
		cluster.revalidate();
		cluster.repaint();
	}

	private DispersionkMeanPanel panelDisp;
	private DispersionFuzzyPanel panelDispF;

	private JTable getClusterTable() {
		if (session.getListOfClustering().get(clusteringTable.getSelectedRow()).getClusteringType() == KMEANS) {
			clusterTable = new ClusterTableKMeans(new ClusterTableModelKMeans());
			clusterTable.setFont(new Font(clusterTable.getFont().getName(), clusterTable.getFont().getStyle(), clusterTable.getFont().getSize() + 1));
			clusterTable.setSelectionBackground(new Color(83, 126, 126));
			clusterTable.setSelectionForeground(Color.BLACK);
			clusterTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			clusterTable.setBorder(BorderFactory.createLoweredBevelBorder());
			clusterTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			clusterTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (!e.getValueIsAdjusting()) {
						//Se entrambe le tabelle sono selezionate
						if (clusterTable.getSelectedRow() >= 0) {
							clusingP.removeAll();
							clustP.removeAll();
							if (changedType) {
								matrixPanel.removeAll();
							}
							switch (session.getListOfClustering().get(clusteringTable.getSelectedRow()).getClusteringType()) {
								case KMEANS:
									clusingP.add(new KMeansInfoClustering((ClusteringInfosKMean) ((KMean) session.getListOfClustering().get(
											clusteringTable.getSelectedRow())).getClusteringInfos(clusterTable.getSelectedRow() + 1), clusterTable
											.getSelectedRow() + 2, model));
									clustP.add(new KMeansInfoCluster((ClusteringInfosKMean) ((KMean) session.getListOfClustering().get(
											clusteringTable.getSelectedRow())).getClusteringInfos(clusterTable.getSelectedRow() + 1), clusterTable
											.getSelectedRow() + 2, session, clusteringTable.getSelectedRow()));
									if (changedType) {
										//matrix = new DispKMeanMatrix(model,-1);
										//matrixPanel.add(matrix);
										panelDisp = new DispersionkMeanPanel(session, clusteringTable.getSelectedRow(),
												clusterTable.getSelectedRow() + 1);
										matrixPanel.add(panelDisp);
									}
									panelDisp.setClustering(clusterTable.getSelectedRow() + 1);
									//matrix.setClustering(clusteringTable.getSelectedRow(),clusterTable.getSelectedRow()+1);
									break;
							}
							matrixPanel.revalidate();
							matrixPanel.repaint();
							clusingP.revalidate();
							clusingP.repaint();
							clustP.revalidate();
							clustP.repaint();
						} else {
							if (clusterTable.getRowCount() > 0) {
								clusterTable.setRowSelectionInterval(0, 0);
							}
						}
					}
				}
			});
			return clusterTable;
		}
		if (session.getListOfClustering().get(clusteringTable.getSelectedRow()).getClusteringType() == FUZZYK) {
			clusterTable = new ClusterTableFuzzy(new ClusterTableModelFuzzy());
			clusterTable.setFont(new Font(clusterTable.getFont().getName(), clusterTable.getFont().getStyle(), clusterTable.getFont().getSize() + 1));
			clusterTable.setSelectionBackground(new Color(83, 126, 126));
			clusterTable.setSelectionForeground(Color.BLACK);
			clusterTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			clusterTable.setBorder(BorderFactory.createLoweredBevelBorder());
			clusterTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			clusterTable.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
				public void valueChanged(ListSelectionEvent e) {
					if (!e.getValueIsAdjusting()) {
						//Se entrambe le tabelle sono selezionate
						if (clusterTable.getSelectedRow() >= 0) {
							clusingP.removeAll();
							clustP.removeAll();
							if (changedType) {
								matrixPanel.removeAll();
							}
							switch (session.getListOfClustering().get(clusteringTable.getSelectedRow()).getClusteringType()) {
								case FUZZYK:
									ClusteringInfosFuzzy infos = ((ClusteringInfosFuzzy) ((FuzzyKMean) session.getListOfClustering().get(
											clusteringTable.getSelectedRow())).getClusteringInfos(clusterTable.getSelectedRow()));
									if (infos.getError() == -1) {
										infos.setError(model.getMatrix(), 0.1);
									}
									if (changedType) {
										panelDispF = new DispersionFuzzyPanel(session, clusteringTable.getSelectedRow(), clusterTable
												.getSelectedRow());
										//matrix2 = new DispFuzzyMatrix(model,-1);
										//matrixPanel.add(matrix2);
										matrixPanel.add(panelDispF);
									}
									//matrix2.setClustering(clusteringTable.getSelectedRow(),clusterTable.getSelectedRow());
									panelDispF.setClustering(clusterTable.getSelectedRow());
									FuzzyInfoClustering ficg = new FuzzyInfoClustering(model.getMatrix(), panelDispF.getMatrix(), clusterTable
											.getSelectedRow() + 2, ((FuzzyKMean) session.getListOfClustering().get(clusteringTable.getSelectedRow()))
											.getEntropy()[clusterTable.getSelectedRow()], infos);
									clusingP.add(ficg);
									FuzzyInfoCluster fic = new FuzzyInfoCluster(infos, clusterTable.getSelectedRow() + 2, session, clusteringTable
											.getSelectedRow());
									ficg.setPanelCluster(fic);
									clustP.add(fic);
									tabbed.setSelectedIndex(0);
									break;
							}
							matrixPanel.revalidate();
							matrixPanel.repaint();
							clusingP.revalidate();
							clusingP.repaint();
							clustP.revalidate();
							clustP.repaint();
						} else {
							if (clusterTable.getRowCount() > 0) {
								clusterTable.setRowSelectionInterval(0, 0);
							}
						}
					}
				}
			});
			return clusterTable;
		}
		return null;
	}

	@Override
	public void gotFocus() {
		((ClusteringTableModel) clusteringTable.getModel()).setClustering(session.getListOfClustering());
		if (clusteringTable.getRowCount() > 0) {
			clusteringTable.setRowSelectionInterval(0, 0);
		}
		parent.setCurrentPanel(WORKLOAD_INPUT_PANEL);
	}

	private class ClusteringTableModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] header = { "Clustering", "Cl.", "" };
		private Vector<Clustering> clusterings = null;

		public int getColumnCount() {
			return header.length;
		}

		public int getRowCount() {
			return clusterings.size();
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < clusterings.size()) {
				switch (columnIndex) {
					case 0:
						return "  " + clusterings.get(rowIndex).getName();
					case 1:
						return Integer.toString(clusterings.get(rowIndex).getNumCluster());
					default:
						return null;
				}
			}
			return null;
		}

		public ClusteringTableModel(Vector<Clustering> cl) {
			clusterings = cl;
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
			if (columnIndex == 2) {
				return true;
			}
			return false;
		}

		@Override
		public Class getColumnClass(int index) {
			if (index == 2) {
				return JButton.class;
			}
			return String.class;
		}

		public void setClustering(Vector<Clustering> cl) {
			clusterings = cl;
			fireTableDataChanged();
		}
	}

	private class ClusterTableFuzzy extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public ClusterTableFuzzy(AbstractTableModel model) {
			super(model);
			getColumnModel().getColumn(0).setPreferredWidth(20);
			getColumnModel().getColumn(1).setPreferredWidth(75);
			getColumnModel().getColumn(2).setPreferredWidth(75);
		}
	}

	private class ClusterTableModelFuzzy extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] header = { "Cl", "Entropy", "Ratio" };
		private double[] entropy;

		public int getColumnCount() {
			return header.length;
		}

		public int getRowCount() {
			return entropy.length;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < entropy.length) {
				switch (columnIndex) {
					case 0:
						return new Integer(rowIndex + 2);
					case 1:
						return defaultFormat.format(entropy[rowIndex]);
					case 2:
						if (rowIndex == 0) {
							return " -";//defaultFormat.format(Double.NaN); 
						}
						return defaultFormat.format(entropy[rowIndex] / entropy[rowIndex - 1]);
					default:
						return null;
				}
			}
			return null;
		}

		public ClusterTableModelFuzzy(double[] cl) {
			entropy = cl;
		}

		public ClusterTableModelFuzzy() {
			entropy = new double[0];
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
			if (index == 0) {
				return Integer.class;
			}
			return String.class;
		}

		public void setCluster(double[] cl) {
			entropy = cl;
			fireTableDataChanged();
		}
	}

	private class ClusterTableKMeans extends JTable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public ClusterTableKMeans(AbstractTableModel model) {
			super(model);
			getColumnModel().getColumn(0).setPreferredWidth(20);
			getColumnModel().getColumn(1).setPreferredWidth(20);
			getColumnModel().getColumn(2).setPreferredWidth(75);
			getColumnModel().getColumn(3).setPreferredWidth(75);
		}
	}

	private class ClusterTableModelKMeans extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] header = { "Cl", "G", "OMSR", "Ratio" };
		private ClusteringInfosKMean[] clusters = null;

		public int getColumnCount() {
			return header.length;
		}

		public int getRowCount() {
			return clusters.length - 1;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex < clusters.length) {
				switch (columnIndex) {
					case 0:
						return new Integer(rowIndex + 2);
					case 1:
						if ((clusters[rowIndex + 1]).isGoodCluster == 1) {
							return JMTImageLoader.loadImage("Measure_ok", new Dimension((int) (BUTTONSIZE * 0.5), (int) (BUTTONSIZE * 0.5)));
						} else {
							return JMTImageLoader.loadImage("Measure_fail", new Dimension((int) (BUTTONSIZE * 0.5), (int) (BUTTONSIZE * 0.5)));
						}
					case 2:
						return defaultFormat.format((clusters[rowIndex + 1]).omsr);
					case 3:
						return defaultFormat.format((clusters[rowIndex + 1]).ratio);
					default:
						return null;
				}
			}
			return null;
		}

		public ClusterTableModelKMeans(ClusteringInfosKMean[] cl) {
			clusters = cl;
		}

		public ClusterTableModelKMeans() {
			clusters = new ClusteringInfosKMean[0];
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
			if (index == 0) {
				return Integer.class;
			}
			if (index == 1) {
				return ImageIcon.class;
			}
			return String.class;
		}

		public void setCluster(ClusteringInfosKMean[] cl) {
			clusters = cl;
			fireTableDataChanged();
		}
	}

	private static final String helpText = "<HTML>" + "This panel shows clustering results information.<br>"
			+ "<UL><LI>Clustering panel shows statistics about how data are aprtitioned in clusters.<br></LI>"
			+ "<LI>Cluster panel shows statistics and graphs of a single cluster.<br></LI>"
			+ "<LI>Scatter plot matrix panel show every variable vs. variable graphs and can be enlarged<p>with double click.<br></LI>" + "</HTML>";

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);
	}

	@Override
	public void lostFocus() {
		parent.setLastPanel(WORKLOAD_INFOCLUSTERING_PANEL);
	}
}
