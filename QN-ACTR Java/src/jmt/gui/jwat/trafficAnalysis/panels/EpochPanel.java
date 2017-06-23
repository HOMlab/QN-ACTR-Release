package jmt.gui.jwat.trafficAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import jmt.engine.jwat.trafficAnalysis.ModelTrafficAnalysis;
import jmt.engine.jwat.trafficAnalysis.OnResetModel;
import jmt.engine.jwat.trafficAnalysis.TrafficAnalysisSession;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.JWatWizard;
import jmt.gui.jwat.MainJwatWizard;

public class EpochPanel extends WizardPanel implements JWATConstants {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private MainJwatWizard burstwizard = null;
	private JSpinner epochs = null;
	private ModelTrafficAnalysis model = null;
	private TrafficAnalysisSession session = null;
	private boolean canGoOn = false;

	public EpochPanel(MainJwatWizard burstwizard) {
		super();
		this.burstwizard = burstwizard;
		model = (ModelTrafficAnalysis) burstwizard.getModel();
		session = (TrafficAnalysisSession) burstwizard.getSession();

		initComponents();
		((ModelTrafficAnalysis) this.burstwizard.getModel()).addResetModelListener(new OnResetModel() {
			public void modelResetted() {
				EpochPanel.this.removeAll();
				initComponents();
			}
		});
	}

	private void initComponents() {
		this.setLayout(new BorderLayout());
		epochs = new JSpinner(new SpinnerNumberModel(10, 10, 50, 1));
		JPanel epochOption = new JPanel(new BorderLayout());
		JPanel flowTemp = new JPanel(new FlowLayout(FlowLayout.LEFT));
		epochs.setPreferredSize(new Dimension(70, 40));
		epochs.setFont(new Font(epochs.getFont().getName(), epochs.getFont().getStyle(), epochs.getFont().getSize() + 4));
		flowTemp.add(new JLabel("<html><body><h3>Select the maximum number of epochs: </h3></body></html> "));
		flowTemp.add(epochs);
		JButton setEpoch = new JButton(this.setEpoch);
		setEpoch.setPreferredSize(new Dimension(85, 35));
		flowTemp.add(setEpoch);
		epochOption.add(flowTemp, BorderLayout.CENTER);
		//JPanel btnPanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		//btnPanel.add(setEpoch);
		//epochOption.add(btnPanel,BorderLayout.SOUTH);
		this.add(epochOption, BorderLayout.NORTH);
	}

	@Override
	public String getName() {
		return "Epoch";
	}

	private Action setEpoch = new AbstractAction("Set epoch") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			session.setParameters(((Integer) epochs.getValue()).intValue());
			((JWatWizard) getParentWizard()).setEnableButton("Next >", true);
			((JWatWizard) getParentWizard()).setEnableButton("Solve", false);
			canGoOn = true;
			((JWatWizard) getParentWizard()).showNextPanel();
		}
	};

	//	 TODO controllare validita dei dati forniti nel pannello e creazione e passaggio informazioni al modello per il prossimo panello
	// Chiamata prima di passare al prossimo pannello
	@Override
	public boolean canGoForward() {
		return canGoOn;
	}

	// TODO controllare con Fuma cosa fare
	// Chiamata quando dal pannello si torna indietro
	@Override
	public boolean canGoBack() {
		return true;
	}

	@Override
	public void lostFocus() {
		burstwizard.setLastPanel(TRAFFIC_EPOCH_PANEL);
	}
}
