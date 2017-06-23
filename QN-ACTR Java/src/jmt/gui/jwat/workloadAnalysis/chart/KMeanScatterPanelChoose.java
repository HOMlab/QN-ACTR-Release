package jmt.gui.jwat.workloadAnalysis.chart;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;

/**
 * Pannello per la visualizzazione dello scatter plot risultato KMean
 * @author Brambilla Davide matr 667986, Fumagalli Claudio 667971
 */
public class KMeanScatterPanelChoose extends JPanel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private JComboBox varXCombo;
	private JComboBox varYCombo;
	private ModelWorkloadAnalysis model;
	private WorkloadAnalysisSession session;

	private int clustering;
	private int clust;

	protected AbstractAction VIS_SCATTER = new AbstractAction("Scatter") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			JFrame f = new JFrame();
			f.setSize(600, 620);
			KMeanScatter s = new KMeanScatter(varXCombo.getSelectedIndex(), varYCombo.getSelectedIndex(), KMeanScatterPanelChoose.this.session, f,
					clustering, clust);
			f.setContentPane(s);
			f.setVisible(true);
		}
	};

	public KMeanScatterPanelChoose(WorkloadAnalysisSession m) {
		super(new BorderLayout());
		setBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Scatter Clustering"));

		model = (ModelWorkloadAnalysis) m.getDataModel();
		this.session = m;

		varXCombo = new JComboBox(model.getMatrix().getVariableNames());
		varYCombo = new JComboBox(model.getMatrix().getVariableNames());
		varXCombo.setSelectedIndex(0);
		varYCombo.setSelectedIndex(1);

		JButton vis = new JButton(VIS_SCATTER);

		JPanel combos = new JPanel(new GridLayout(1, 2, 5, 0));
		combos.add(varXCombo);
		combos.add(varYCombo);

		add(combos, BorderLayout.NORTH);
		add(vis, BorderLayout.SOUTH);
	}

	public void setClusteringInfos(int clustering, int clust) {
		this.clust = clust;
		this.clustering = clustering;
	}
}
