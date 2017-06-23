package jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import jmt.engine.jwat.DinamicFormat;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.ClusteringInfosKMean;
import jmt.engine.jwat.workloadAnalysis.utils.JavaWatColor;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.jwat.JWATConstants;

public class KMeansInfoClustering extends JPanel implements JWATConstants, CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private ClusteringInfosKMean info;
	private int numClust;
	private ModelWorkloadAnalysis model;
	private final String OMSR_START_TEXT = HTML_START + HTML_FONT_TITLE + "Omsr value " + HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM;
	private final String OMSR_END_TEXT = HTML_FONT_NOR_END + HTML_FONT_SMALL + "(Overall Mean Square Ratio): " + HTML_FONT_NOR_END + HTML_FONT_NORM;
	private final String RATIO_START_TEXT = HTML_START + HTML_FONT_TITLE + "Ratio value: " + HTML_FONT_TIT_END_NO_CAPO + HTML_FONT_NORM;
	private JTable tableClusters = null;
	private JTable tableVars = null;
	//private JComboBox comboVars = null;
	private JList listVars = null;
	private pieChartIcon pieChartVars = null;
	private JLabel chartVars = null;

	public KMeansInfoClustering(ClusteringInfosKMean info, int i, ModelWorkloadAnalysis m) {
		this.info = info;
		numClust = i;
		model = m;
		initPanel();
	}

	private void initPanel() {
		Box mainPanel = Box.createHorizontalBox();
		Box allPanel = Box.createVerticalBox();
		mainPanel.add(Box.createHorizontalStrut(5));
		mainPanel.add(allPanel);
		mainPanel.add(Box.createHorizontalStrut(5));

		//this.setLayout(new BorderLayout());
		this.setLayout(new GridLayout(1, 1));
		this.add(mainPanel);

		JPanel constr = new JPanel(new BorderLayout());

		/** Pannello informazione clustering **/
		JPanel north = new JPanel(new FlowLayout(FlowLayout.LEFT, 20, 0));
		north.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "General information on clustering"));

		JLabel res = new JLabel();
		res.setPreferredSize(new Dimension((int) (BUTTONSIZE * 1.5), (int) (BUTTONSIZE * 1.5)));
		if (info.isGoodCluster == 1) {
			res.setIcon(JMTImageLoader.loadImage("Measure_ok", new Dimension((int) (BUTTONSIZE * 1.5), (int) (BUTTONSIZE * 1.5))));
		} else {
			res.setIcon(JMTImageLoader.loadImage("Measure_fail", new Dimension((int) (BUTTONSIZE * 1.5), (int) (BUTTONSIZE * 1.5))));
		}
		north.add(res);

		JPanel p = new JPanel(new GridLayout(2, 1, 5, 5));
		JLabel omsr = new JLabel(OMSR_START_TEXT + OMSR_END_TEXT + defaultFormat.format(info.omsr) + HTML_FONT_NOR_END + HTML_END);
		JLabel ratio = new JLabel(RATIO_START_TEXT + defaultFormat.format(info.ratio) + HTML_FONT_NOR_END + HTML_END);
		p.add(omsr);
		p.add(ratio);
		north.add(p, BorderLayout.CENTER);
		//this.add(north,BorderLayout.NORTH);
		constr.add(north, BorderLayout.NORTH);

		/** Pannello centrale **/
		JPanel center = new JPanel(new GridLayout(2, 1));

		/** Pannello clusters details **/
		JPanel clustDet = new JPanel(new BorderLayout(15, 5));
		clustDet.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Clusters details"));
		JScrollPane tabCluDetScroll = new JScrollPane(getClustDetTable());
		clustDet.add(tabCluDetScroll, BorderLayout.CENTER);
		clustDet.add(setPie1(), BorderLayout.EAST);

		/** Pannello variables details **/
		JPanel due = new JPanel(new BorderLayout(0, 0));
		due.setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Distribution of the variable values among the clusters"));
		//combo variabili
		JPanel var = new JPanel(new BorderLayout(5, 0));
		chartVars = setPie2(0);
		var.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Variables" + HTML_FONT_TIT_END + HTML_END, SwingConstants.CENTER), BorderLayout.NORTH);
		var.add(getListVars(), BorderLayout.CENTER);
		due.add(var, BorderLayout.WEST);
		//tabella info variabile
		JScrollPane tabVarsDetScroll = new JScrollPane(getVarsDetTable());
		//Update graphic interface
		listVars.clearSelection();
		due.add(tabVarsDetScroll, BorderLayout.CENTER);
		due.add(chartVars, BorderLayout.EAST);

		center.add(clustDet);
		center.add(due);

		//this.add(center,BorderLayout.CENTER);
		constr.add(center, BorderLayout.CENTER);
		allPanel.add(constr);
	}

	private JTable getClustDetTable() {
		tableClusters = new JTable(new clustDetModel(info.numElem, info.percent)) {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			{
				setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
				getColumnModel().getColumn(0).setPreferredWidth(170);
				getColumnModel().getColumn(1).setPreferredWidth(110);
				getColumnModel().getColumn(2).setPreferredWidth(110);
				getColumnModel().getColumn(3).setPreferredWidth(55);
			}

			@Override
			public TableCellRenderer getCellRenderer(int row, int column) {
				if (column == 3) {
					return new ColorRenderer();
				}
				return getDefaultRenderer(String.class);
			}
		};
		tableClusters.setSelectionBackground(new Color(83, 126, 126));
		tableClusters.setSelectionForeground(Color.BLACK);
		tableClusters.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		tableClusters.setFont(new Font(tableClusters.getFont().getName(), tableClusters.getFont().getStyle(), tableClusters.getFont().getSize() + 1));
		tableClusters.setRowSelectionInterval(0, 0);
		return tableClusters;
	}

	private JTable getVarsDetTable() {
		double[] p = new double[numClust];
		for (int i = 0; i < numClust; i++) {
			p[i] = info.infoCluster[i].percVar[listVars.getSelectedIndex()];
		}
		tableVars = new JTable(new varsDetModel(p)) {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;
			{
				setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
				getColumnModel().getColumn(0).setPreferredWidth(170);
				getColumnModel().getColumn(1).setPreferredWidth(100);
				getColumnModel().getColumn(2).setPreferredWidth(55);
			}

			@Override
			public TableCellRenderer getCellRenderer(int row, int column) {
				if (column == 2) {
					return new ColorRenderer();
				}
				return getDefaultRenderer(String.class);
			}
		};
		tableVars.setSelectionBackground(new Color(83, 126, 126));
		tableVars.setSelectionForeground(Color.BLACK);
		tableVars.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		tableVars.setFont(new Font(tableVars.getFont().getName(), tableVars.getFont().getStyle(), tableVars.getFont().getSize() + 1));
		tableVars.setRowSelectionInterval(0, 0);
		return tableVars;
	}

	private JPanel getListVars() {
		/* Versione con combo box
		comboVars = new JComboBox(model.getMatrix().getVariableNames());
		comboVars.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e) {
				if(tableVars != null && comboVars != null){
					System.err.println("Selezione cluster");	
					double[] p = new double[numClust];
					for(int i = 0; i < numClust;i++)
						p[i] = info.infoCluster[i].percVar[comboVars.getSelectedIndex()];
					
					((varsDetModel)tableVars.getModel()).setNewPerc(p);
					tableVars.tableChanged(new TableModelEvent(tableVars.getModel()));
					
					int[] angle=new int[numClust];
			        int i;
			        angle[0]=0;
			        for(i=1;i<numClust;i++) angle[i]=angle[i-1]+(int)((360) * p[i-1]);
			        for(;i<angle.length;i++) angle[i]=360;
			        
					pieChartVars.setAngle(angle); 
					chartVars.repaint();
				}	
			}
		});
		comboVars.setSelectedIndex(0);
		*/
		listVars = new JList(model.getMatrix().getVariableNames());

		listVars.setSelectionBackground(new Color(83, 126, 126));
		listVars.setSelectionForeground(Color.BLACK);

		JScrollPane s = new JScrollPane(listVars, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		s.setPreferredSize(new Dimension(120, 90));
		listVars.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {
					if (listVars.getSelectedIndex() >= 0) {
						if (tableVars != null && listVars != null) {
							double[] p = new double[numClust];
							for (int i = 0; i < numClust; i++) {
								p[i] = info.infoCluster[i].percVar[listVars.getSelectedIndex()];
							}

							((varsDetModel) tableVars.getModel()).setNewPerc(p);
							tableVars.tableChanged(new TableModelEvent(tableVars.getModel()));

							int[] angle = new int[numClust];
							int i;
							angle[0] = 0;
							for (i = 1; i < numClust; i++) {
								angle[i] = angle[i - 1] + (int) ((360) * p[i - 1]);
							}
							for (; i < angle.length; i++) {
								angle[i] = 360;
							}

							pieChartVars.setAngle(angle);
							chartVars.repaint();
						}
					} else {
						listVars.setSelectedIndex(0);
					}
				}
			}
		});
		listVars.setSelectedIndex(0);
		JPanel p = new JPanel();
		p.add(s);
		return p;
	}

	private JLabel setPie1() {
		int[] angle = new int[numClust];
		double[] prc = info.percent;
		int i;

		angle[0] = 0;
		for (i = 1; i < numClust; i++) {
			angle[i] = angle[i - 1] + (int) ((360) * prc[i - 1]);
		}
		for (; i < angle.length; i++) {
			angle[i] = 360;
		}
		final JLabel l = new JLabel();
		final pieChartIcon p = new pieChartIcon(numClust);
		l.addMouseMotionListener(new MouseMotionAdapter() {
			@Override
			public void mouseMoved(MouseEvent e) {
				int pos;

				pos = p.getPosXY(e.getX(), e.getY());

				if (pos != -1) {
					l.setToolTipText(tableClusters.getValueAt(pos, 2).toString());
				} else {
					l.setToolTipText("");
				}
			}
		});
		p.setAngle(angle);
		l.setIcon(p);
		return l;
	}

	private JLabel setPie2(int var) {
		int[] angle = new int[numClust];
		int i;

		angle[0] = 0;
		for (i = 1; i < numClust; i++) {
			angle[i] = angle[i - 1] + (int) ((360) * info.infoCluster[i].percVar[var]);
		}
		for (; i < angle.length; i++) {
			angle[i] = 360;
		}

		final JLabel l = new JLabel();
		pieChartVars = new pieChartIcon(numClust);
		l.addMouseMotionListener(new MouseMotionAdapter() {
			@Override
			public void mouseMoved(MouseEvent e) {
				int pos;

				pos = pieChartVars.getPosXY(e.getX(), e.getY());

				if (pos != -1) {
					l.setToolTipText(tableVars.getValueAt(pos, 1).toString());
				} else {
					l.setToolTipText("");
				}
			}
		});
		pieChartVars.setAngle(angle);
		l.setIcon(pieChartVars);
		return l;
	}

	private class pieChartIcon implements Icon {
		private final int DIAMETRO = 100;
		private int[] angle;
		private double centreX, centreY;

		public pieChartIcon(int numSpicchi) {
			centreX = 0;
			centreY = 0;
			angle = new int[numSpicchi + 1];
		}

		public void setAngle(int[] posAngoli) {
			int i;
			for (i = 0; i < angle.length - 1; i++) {
				angle[i] = posAngoli[i];
			}
			angle[i] = 360;
		}

		public int getIconWidth() {
			return DIAMETRO;
		}

		public int getIconHeight() {
			return DIAMETRO;
		}

		public void paintIcon(Component c, Graphics g, int x, int y) {
			float len = angle.length;

			centreX = x + (DIAMETRO / 2);
			centreY = y + (DIAMETRO / 2);

			for (int i = 0; i < len - 1; i++) {
				g.setColor(JavaWatColor.getColor(i));
				g.fillArc(x, y, DIAMETRO, DIAMETRO, angle[i], angle[i + 1] - angle[i]);
				g.setColor(Color.BLACK);
				g.drawArc(x, y, DIAMETRO, DIAMETRO, angle[i], angle[i + 1] - angle[i]);
			}
		}

		public int getPosXY(int x, int y) {
			int ret = 1;
			double angl;

			if (Math.sqrt(Math.pow((x - centreX), 2) + Math.pow((y - centreY), 2)) > DIAMETRO / 2) {
				return -1;
			}
			//if(x-centreX==0) return 0;
			angl = Math.toDegrees(Math.atan((centreY - y) / (x - centreX)));
			if (x < centreX) {
				angl = 180 + angl;
			}
			if (angl < 0) {
				angl = 360 + angl;
			}
			//System.out.println("ANGOLO= " + angl);
			for (; ret < angle.length; ret++) {
				if (angl < angle[ret]) {
					break;
				}
			}

			return (ret - 1);
		}
	}

	/** MODEL PER TABELLA CLUSTER DETAILS **/
	private class clustDetModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] colHeader = { "Cluster", "Num. observations", "Percentage", "" };
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

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex >= 0) {
				if (columnIndex == 0) {
					return "Cluster " + (rowIndex + 1);
				}
				if (columnIndex == 1) {
					return Integer.toString(Elem[rowIndex]);
				}
				if (columnIndex == 2) {
					return f.format(Perc[rowIndex]);
				}
				if (columnIndex == 3) {
					return JavaWatColor.getColor(rowIndex);
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

	/** MODEL PER TABELLA VARIABLES DETAILS **/
	private class varsDetModel extends AbstractTableModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private String[] colHeader = { "Cluster", "Perc", "" };
		private double[] Perc = null;
		DinamicFormat f = new DinamicFormat("###.###%", 0); //????? CONTROLLARE A COSA E SE SERVE 

		public varsDetModel(double perc[]) {
			Perc = perc;
		}

		public void setNewPerc(double[] p) {
			Perc = p;
		}

		public int getRowCount() {
			return Perc.length;
		}

		public int getColumnCount() {
			return colHeader.length;
		}

		public Object getValueAt(int rowIndex, int columnIndex) {
			if (rowIndex >= 0) {
				if (columnIndex == 0) {
					return "Cluster " + (rowIndex + 1);
				}
				if (columnIndex == 1) {
					return f.format(Perc[rowIndex]); //HA dato errore 35° cluster
				}
				if (columnIndex == 2) {
					return JavaWatColor.getColor(rowIndex);
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

	private class ColorRenderer extends JLabel implements TableCellRenderer {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public ColorRenderer() {
			setOpaque(true); //MUST do this for background to show up.
			setBorder(BorderFactory.createMatteBorder(2, 5, 2, 5, Color.WHITE));
		}

		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			setBackground((Color) value);
			return this;
		}

	}
}
