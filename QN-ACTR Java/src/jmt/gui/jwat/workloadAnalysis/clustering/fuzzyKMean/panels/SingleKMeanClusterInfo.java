package jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ScrollPaneConstants;
import javax.swing.WindowConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import jmt.engine.jwat.Observation;
import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.clustering.kMean.KMean;
import jmt.engine.jwat.workloadAnalysis.utils.JWatWorkloadManager;
import jmt.engine.jwat.workloadAnalysis.utils.JavaWatColor;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.workloadAnalysis.chart.KMeanSingleClusterScatter;

public class SingleKMeanClusterInfo extends JPanel implements CommonConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JButton loadInfo;
	private JButton saveInfo;
	private JTextArea infoCluster;
	private ModelWorkloadAnalysis model;
	private WorkloadAnalysisSession session;

	private JComboBox xVar;
	private JComboBox yVar;
	private SingleScatter panel;
	private int cluster = -1;
	private int match = 0;
	private int clustering;
	private boolean redraw = true;

	public SingleKMeanClusterInfo(WorkloadAnalysisSession m, int clustering, int cluster) {
		this.model = (ModelWorkloadAnalysis) m.getDataModel();
		this.session = m;
		this.clustering = clustering;
		this.cluster = cluster;
		initPanel();
	}

	public void setCluster(int value) {
		this.match = value;
		infoCluster.setText("");
		redraw = true;
		panel.repaint();
	}

	private void initPanel() {
		setLayout(new BorderLayout());
		setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Single cluster details"));
		panel = new SingleScatter();

		infoCluster = new JTextArea();
		infoCluster.setEditable(false);
		infoCluster.setFont(new Font(infoCluster.getFont().getName(), infoCluster.getFont().getStyle(), infoCluster.getFont().getSize() + 1));

		JScrollPane sp = new JScrollPane(infoCluster, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		loadInfo = new JButton("Show Obser.");
		loadInfo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				int ret = JOptionPane.showConfirmDialog(SingleKMeanClusterInfo.this, "This operation may require several minutes, continue? ",
						"Warning!!", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
				if (ret == JOptionPane.YES_OPTION) {
					Observation[] o = model.getMatrix().getVariables()[0].getCurObs();
					String s = "";
					short[] c1 = ((KMean) session.getListOfClustering().get(clustering)).getAsseg()[cluster - 1];
					for (Observation element : o) {
						if (c1[element.getID() - 1] == match) {
							s += element.toString() + "\n";
						}
					}
					infoCluster.setText(s);
				}
			}
		});
		saveInfo = new JButton("Save Info");

		JPanel combos = new JPanel(new GridLayout(2, 1));
		xVar = new JComboBox(model.getMatrix().getVariableNames());
		yVar = new JComboBox(model.getMatrix().getVariableNames());
		xVar.setSelectedIndex(0);
		if (model.getMatrix().getVariableNames().length > 1) {
			yVar.setSelectedIndex(1);
		} else {
			yVar.setSelectedIndex(0);
		}
		xVar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				redraw = true;
				panel.repaint();
			}
		});
		yVar.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				redraw = true;
				panel.repaint();
			}
		});
		xVar.setPreferredSize(new Dimension(80, 18));
		JPanel uno = new JPanel();
		uno.add(new JLabel("X: "));
		uno.add(xVar);
		JPanel due = new JPanel();
		due.add(new JLabel("Y: "));
		yVar.setPreferredSize(new Dimension(80, 18));
		due.add(yVar);
		combos.add(uno);
		combos.add(due);

		JPanel left = new JPanel(new BorderLayout());
		JPanel graph = new JPanel();

		JPanel t = new JPanel(new BorderLayout());
		JPanel buttons = new JPanel(new FlowLayout());
		loadInfo.setPreferredSize(DIM_BUTTON_S);
		loadInfo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

			}
		});
		buttons.add(loadInfo);

		saveInfo.setPreferredSize(DIM_BUTTON_S);
		saveInfo.setEnabled(false);
		saveInfo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

			}
		});
		buttons.add(saveInfo);

		t.add(sp, BorderLayout.CENTER);
		t.add(buttons, BorderLayout.SOUTH);

		add(t, BorderLayout.CENTER);
		left.add(panel, BorderLayout.CENTER);
		left.add(combos, BorderLayout.SOUTH);
		add(left, BorderLayout.WEST);
	}

	private class SingleScatter extends JPanel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		private int WIDTH = 100;
		private int HEIGHT = 100;
		private BufferedImage graph = new BufferedImage(WIDTH, HEIGHT, BufferedImage.TYPE_INT_RGB);

		public SingleScatter() {
			setPreferredSize(new Dimension(WIDTH + 4, HEIGHT + 4));
			setCursor(new Cursor(Cursor.HAND_CURSOR));
			addMouseListener(new MouseListener() {

				public void mouseClicked(MouseEvent e) {
					if (e.getClickCount() == 2) {
						final JFrame f = new JFrame();

						JWatWorkloadManager.addJMTWindow(f);
						f.addWindowListener(new WindowAdapter() {
							@Override
							public void windowClosing(WindowEvent e) {
								JWatWorkloadManager.exit(f);
							}

							@Override
							public void windowClosed(WindowEvent e) {
								JWatWorkloadManager.exit(f);
							}
						});

						f.setSize(640, 690);
						f.setTitle("Single cluster K-Means scatter plot");
						f.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
						f.setContentPane(new KMeanSingleClusterScatter(xVar.getSelectedIndex(), yVar.getSelectedIndex(), session, f, clustering,
								cluster - 1, match));
						f.setVisible(true);

					}
				}

				public void mouseEntered(MouseEvent e) {
				}

				public void mouseExited(MouseEvent e) {
				}

				public void mousePressed(MouseEvent e) {
				}

				public void mouseReleased(MouseEvent e) {
				}

			});
		}

		@Override
		public void paint(Graphics g) {
			super.paint(g);
			//Draw Black rectangle
			Graphics2D g1 = (Graphics2D) g;
			//Sfondo Bianco
			g1.setColor(Color.WHITE);
			g1.fillRect(0, 0, WIDTH + 3, HEIGHT + 3);
			//Bordi neri
			g1.setColor(Color.BLACK);
			g1.drawLine(1, 1, 1, HEIGHT + 1);
			g1.drawLine(1, 1, WIDTH + 1, 1);
			g1.drawLine(1, HEIGHT + 1, WIDTH + 1, HEIGHT + 1);
			g1.drawLine(WIDTH + 1, 1, WIDTH + 1, HEIGHT + 1);
			if (redraw) {
				Graphics2D g2 = (Graphics2D) graph.getGraphics();
				VariableNumber x = model.getMatrix().getVariables()[xVar.getSelectedIndex()];
				VariableNumber y = model.getMatrix().getVariables()[yVar.getSelectedIndex()];
				int row = yVar.getSelectedIndex();
				short[] c1 = ((KMean) session.getListOfClustering().get(clustering)).getAsseg()[cluster - 1];
				g2.setColor(Color.WHITE);
				g2.fillRect(1, 1, WIDTH - 1, HEIGHT - 1);
				//Calcolo del passo del grafico
				double yFoot = (HEIGHT - 1) / y.getUniStats().getRangeValue();
				for (int i = 1; i <= WIDTH - 1; i++) {
					boolean[] done = new boolean[101];
					int k = 1;
					//System.err.println("STart interval : " + x.getStartInt(i) + " end : " + x.getEndInt(i));
					for (int j = x.getStartInt(i); j < x.getEndInt(i); j++) {
						if ((int) ((x.getValue(j, row) - y.getUniStats().getMinValue()) * yFoot) >= 0) {
							//System.err.println(x.getValue(j,row) +" " + y.getUniStats().getMinValue() +" " + yFoot);
							if (!done[(int) ((x.getValue(j, row) - y.getUniStats().getMinValue()) * yFoot)]) {
								if (c1[x.getObsID(j) - 1] == match) {
									g2.setColor(JavaWatColor.getColor(c1[x.getObsID(j) - 1]));
									g2.fillOval(i, HEIGHT - ((int) (((x.getValue(j, row) - y.getUniStats().getMinValue())) * yFoot)), 1, 1);
								}
								done[(int) ((x.getValue(j, row) - y.getUniStats().getMinValue()) * yFoot)] = true;
							}
						}
					}
				}
				redraw = false;
				repaint();
			}
			g1.drawImage(graph, 1, 1, null);

		}
	}
}
