package jmt.gui.jwat.workloadAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.DefaultListSelectionModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.ProgressStatusListener;
import jmt.engine.jwat.input.EventFinishAbort;
import jmt.engine.jwat.input.EventStatus;
import jmt.engine.jwat.workloadAnalysis.WorkloadAnalysisSession;
import jmt.engine.jwat.workloadAnalysis.clustering.EventClusteringDone;
import jmt.engine.jwat.workloadAnalysis.utils.ModelWorkloadAnalysis;
import jmt.engine.jwat.workloadAnalysis.utils.SetMatrixListener;
import jmt.framework.gui.help.HoverHelp;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.common.CommonConstants;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.JWatWizard;
import jmt.gui.jwat.MainJwatWizard;
import jmt.gui.jwat.workloadAnalysis.clustering.fuzzyKMean.panels.FuzzyOptPanel;
import jmt.gui.jwat.workloadAnalysis.clustering.kMean.panels.KMeansOptPanel;

public class ClusterPanel extends WizardPanel implements CommonConstants, JWATConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private final String CLUSTERING_DESCRIPTION = HTML_START + HTML_FONT_TITLE + "Clustering" + HTML_FONT_TIT_END + HTML_FONT_NORM
			+ "Select clustering algorithm, set options and press solve" + HTML_FONT_NOR_END + HTML_END;
	private final String[] clusteringString = { "k-Means", "Fuzzy k-Means" };

	private JPanel optClustering;

	private JList varList;
	private JList clustList;

	private ModelWorkloadAnalysis model = null;
	private WorkloadAnalysisSession session = null;

	private boolean canGoOn;

	private MainJwatWizard parent;
	private HoverHelp help = null;

	public ClusterPanel(MainJwatWizard parent) {
		this.parent = parent;
		this.model = (ModelWorkloadAnalysis) parent.getModel();
		this.session = (WorkloadAnalysisSession) parent.getSession();
		this.help = parent.getHelp();
		canGoOn = false;
		initPanel();
	}

	@Override
	public String getName() {
		return "Clustering";
	}

	private void initPanel() {
		this.setLayout(new BorderLayout());
		/*Creating Box*/
		Box mainbox = Box.createVerticalBox();
		mainbox.add(Box.createVerticalStrut(20));

		Box paneldesclabel = Box.createHorizontalBox();
		Box centralpanel = Box.createHorizontalBox();

		mainbox.add(paneldesclabel);
		mainbox.add(Box.createVerticalStrut(15));
		mainbox.add(centralpanel);
		mainbox.add(Box.createVerticalStrut(15));

		/** DESCRIPTION LABEL OF PANEL **/
		JPanel jpdesclabel = new JPanel(new GridLayout(1, 1));
		jpdesclabel.add(new JLabel(CLUSTERING_DESCRIPTION));
		paneldesclabel.add(jpdesclabel);

		/** CENTRAL PANEL **/
		JPanel jcentralp = new JPanel(new BorderLayout(10, 0));

		/** SAMPLING OPTION PANEL **/
		optClustering = new JPanel();
		optClustering.setBorder(new TitledBorder(new TitledBorder(new EtchedBorder(EtchedBorder.LOWERED), "Clustering options")));
		optClustering.setLayout(new BorderLayout());

		/** LISTS **/
		JPanel jlistpanel = new JPanel(new GridLayout(1, 2, 10, 5));
		JPanel varpanel = new JPanel(new BorderLayout());
		JPanel cluspanel = new JPanel(new BorderLayout());
		/*PANEL LIST VAR*/
		varpanel.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Variables" + HTML_FONT_TIT_END + HTML_END), BorderLayout.NORTH);
		varList = new JList();

		varList.setSelectionBackground(new Color(83, 126, 126));
		varList.setSelectionForeground(Color.BLACK);

		Font f = varList.getFont();
		varList.setFont(new Font(f.getFontName(), f.getStyle(), f.getSize() + 2));
		varList.setBorder(BorderFactory.createLoweredBevelBorder());
		//varList.getSelectionModel().setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		//varList.setPreferredSize(new Dimension(140, 380));
		varpanel.add(new JScrollPane(varList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED),
				BorderLayout.CENTER);
		/*PANEL LIST CLUSTRING*/
		cluspanel.add(new JLabel(HTML_START + HTML_FONT_TITLE + "Clustering" + HTML_FONT_TIT_END + HTML_END), BorderLayout.NORTH);
		clustList = new JList();

		clustList.setSelectionBackground(new Color(83, 126, 126));
		clustList.setSelectionForeground(Color.BLACK);
		clustList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

		clustList.setFont(new Font(f.getFontName(), f.getStyle(), f.getSize() + 2));
		clustList.setBorder(BorderFactory.createLoweredBevelBorder());
		//clustList.setPreferredSize(new Dimension(140, 380));
		clustList.setListData(clusteringString);
		clustList.addListSelectionListener(new listClusterListener());
		cluspanel.add(
				new JScrollPane(clustList, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED),
				BorderLayout.CENTER);

		jlistpanel.add(cluspanel);
		jlistpanel.add(varpanel);

		jcentralp.add(jlistpanel, BorderLayout.WEST);
		jcentralp.add(optClustering, BorderLayout.CENTER);

		centralpanel.add(Box.createHorizontalStrut(10));
		centralpanel.add(jcentralp);
		centralpanel.add(Box.createHorizontalStrut(10));

		this.add(mainbox);
		model.addOnSetMatrixObservationListener(new SetMatrixListener() {

			public void onSetMatrixObservation() {
				MatrixOsservazioni m = model.getMatrix();
				String[] varLst = m.getVariableNames();
				varList.setListData(varLst);
				varList.setSelectionModel(new myListSelectionModel(model.getMatrix().getNumVariables()));
			}

			public void onResetMatrixObservation() {
				varList.setListData(new String[0]);
			}

		});
	}

	private class listClusterListener implements ListSelectionListener {

		public void valueChanged(ListSelectionEvent e) {
			if (!e.getValueIsAdjusting()) {
				switch (clustList.getSelectedIndex()) {
					case 0:
						optClustering.removeAll();
						optClustering.add(new KMeansOptPanel(ClusterPanel.this, new loadListener(), model), BorderLayout.CENTER);
						optClustering.revalidate();
						optClustering.repaint();
						if (session.getListOfClustering().size() == 0) {
							canGoOn = false;
						}
						break;
					case 1:
						optClustering.removeAll();
						optClustering.add(new FuzzyOptPanel(ClusterPanel.this, new loadListener(), model), BorderLayout.CENTER);
						optClustering.revalidate();
						optClustering.repaint();
						if (session.getListOfClustering().size() == 0) {
							canGoOn = false;
						}
						break;
					default:
						clustList.setSelectedIndex(0);
				}
			}
		}
	}

	public int[] getVarSelected() {
		return ((myListSelectionModel) varList.getSelectionModel()).getSelected();
		//return varList.getSelectedIndices();
	}

	@Override
	public boolean canGoForward() {
		return canGoOn;
	}

	@Override
	public void gotFocus() {
		int pos = clustList.getSelectedIndex();
		clustList.clearSelection();
		if (pos != -1) {
			clustList.setSelectedIndex(pos);
		} else {
			clustList.setSelectedIndex(0);
		}
		if (session.getListOfClustering().size() > 0) {
			canGoOn = true;
		}
		parent.setCurrentPanel(WORKLOAD_INPUT_PANEL);
	}

	private class loadListener implements ProgressStatusListener {

		public void statusEvent(EventStatus e) {

			switch (e.getType()) {
				case EventStatus.ABORT_EVENT:
					abortEvent((EventFinishAbort) e);
					break;
				case EventStatus.DONE_EVENT:
					finishedEvent((EventClusteringDone) e);
					break;
			}
		}

		//Abort caricamento file input
		private void abortEvent(EventFinishAbort e) {
			JOptionPane.showMessageDialog(ClusterPanel.this, e.getMessage(), "CLUSTERING ABORTED!!", JOptionPane.WARNING_MESSAGE);
			canGoOn = false;
			if (session.getListOfClustering().size() == 0) {
				((JWatWizard) getParentWizard()).setEnableButton("Next >", false);
			} else {
				((JWatWizard) getParentWizard()).setEnableButton("Next >", true);
			}
			((JWatWizard) getParentWizard()).setEnableButton("Solve", true);
		}

		//dati caricati
		private void finishedEvent(EventClusteringDone e) {
			//	int confirm=JOptionPane.showConfirmDialog(ClusterPanel.this,"Clustering completed, continue?","CLUSTERING COMPLETED!!",JOptionPane.YES_NO_OPTION,JOptionPane.QUESTION_MESSAGE);
			//	if (confirm==JOptionPane.YES_OPTION){
			session.addClustering(e.getClustering());
			((JWatWizard) getParentWizard()).setEnableButton("Next >", true);
			((JWatWizard) getParentWizard()).setEnableButton("Solve", false);
			canGoOn = true;
			((JWatWizard) getParentWizard()).showNextPanel();
			//	}
			/*	else{
					canGoOn=false;
					System.gc();
					((JWatWizard)getParentWizard()).setEnableButton("Next >",false);
					((JWatWizard)getParentWizard()).setEnableButton("Solve",true);
				}*/
		}
	}

	private static final String helpText = "<HTML>" + "In this panel you can choose which clustering algorithm apply to data.<br><br>"
			+ "<b>K-Means options:</b> select maximum number of clusters and maximum number of iterations.<p>"
			+ "<b>Fuzzy K-Means options:</b> select maximum numver of cluster, maximum number of iteration<p>and fuzziness level.<p><p>"
			+ "You can apply temporary transformation to selected variables (<I>Recommanded</I>).<br>"
			+ "Press solve button to start clustering algorithm.</html>";

	@Override
	public void help() {
		JOptionPane.showMessageDialog(this, helpText, "Help", JOptionPane.INFORMATION_MESSAGE);
	}

	@Override
	public void lostFocus() {
		parent.setLastPanel(WORKLOAD_CLUSTERING_PANEL);
	}

	private class myListSelectionModel extends DefaultListSelectionModel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		boolean[] isSelected;
		int numSelected;

		public myListSelectionModel(int num) {
			this.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
			isSelected = new boolean[num];
			numSelected = 0;
		}

		@Override
		public boolean isSelectedIndex(int index) {
			//System.out.println("IS");
			return isSelected[index];
		}

		@Override
		public void setSelectionInterval(int index0, int index1) {
			if (isSelected[index0]) {
				isSelected[index0] = false;
				numSelected--;
			} else {
				numSelected++;
				isSelected[index0] = true;
			}

			varList.revalidate();
			varList.repaint();
		}

		public int getIndexSelected(int skip) {
			int i;

			for (i = 0; i < isSelected.length; i++) {
				if (isSelected[i]) {
					if (skip == 0) {
						break;
					} else {
						skip--;
					}
				}
			}
			return i;
		}

		public int getNumSelected() {
			return numSelected;
		}

		public boolean[] getBoolSel() {
			return isSelected;
		}

		public int[] getSelected() {
			int res[] = new int[numSelected];
			int pos = 0;
			for (int i = 0; i < isSelected.length; i++) {
				if (isSelected[i]) {
					res[pos++] = i;
				}
			}
			return res;
		}
	}
}
