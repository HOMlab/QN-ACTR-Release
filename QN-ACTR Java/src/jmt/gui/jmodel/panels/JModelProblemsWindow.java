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

package jmt.gui.jmodel.panels;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.border.LineBorder;

import jmt.gui.common.CommonConstants;
import jmt.gui.common.controller.ModelChecker;
import jmt.gui.common.definitions.GuiInterface;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title:</p>
 * <p>Description:</p>
 *
 * @author Francesco D'Aquino
 *         Date: 13-ott-2005
 *         Time: 14.36.31
 *         
 * Modified by Ashanka (May 2010): 
 * Patch: Multi-Sink Perf. Index 
 * Description: Added new Performance index for the capturing the 
 * 				1. global response time (ResponseTime per Sink)
 *              2. global throughput (Throughput per Sink)
 *              each sink per class.
 * Hence new validations are required to check the Performance Indices of
 * response per sink and throughput per sink follow the model validations.
 * 1. Response Time per Sink and Throughput per Sink should have a sink in
 * the model : added new function : isThereSinkPerfIndicesError
 * 2. Response Time per Sink and Throughput per Sink should not be selected
 * with a closed class because for a closed model as of now in JMT no jobs 
 * are routed to the it. So sink should be choosen only when a open class 
 * is present : isSinkPerfIndicesWithClosedClassError.
 * 
 * Modified by Ashanka (June 2010):
 * Updated the Manage Probabilities. Closed Classes routed to Sink with probability <> 0.0 should show warning to Users.
 */
public class JModelProblemsWindow extends JDialog {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	GuiInterface gi;

	private boolean canBeRun;
	private boolean operationCanceled;

	boolean isToJMVAConversion;

	JLabel title;
	JList problemsList;
	ModelChecker mc;
	Vector<ProblemElement> problems;

	GridBagLayout gblayout;
	GridBagConstraints gbconstants;

	JButton continueButton;
	JButton cancelButton;

	JButton typeButton;
	JButton descriptionButton;

	public JModelProblemsWindow(Frame owner, ModelChecker checker, GuiInterface gi) {
		super(owner, true);
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		problems = new Vector<ProblemElement>(0, 1);
		mc = checker;
		isToJMVAConversion = mc.isToJMVA();
		this.gi = gi;
		canBeRun = false;
		operationCanceled = true;
		GridBagLayout gblayout = new GridBagLayout();
		GridBagConstraints gbconstants = new GridBagConstraints();
		getContentPane().setLayout(gblayout);

		if (isToJMVAConversion) {
			this.setTitle("Problems while trying to convert to JMVA");
		} else {
			this.setTitle("Simulation diagnostic");
		}
		setBounds(20, 20, 20, 20);

		title = new JLabel(CommonConstants.HTML_START + CommonConstants.HTML_FONT_TITLE + "Problems found" + CommonConstants.HTML_FONT_TIT_END
				+ CommonConstants.HTML_FONT_NORM + "Click on an element to solve the problem" + CommonConstants.HTML_FONT_NOR_END
				+ CommonConstants.HTML_END);

		problemsList = new JList();
		initializeList();
		problemsList.setListData(problems);
		problemsList.setCellRenderer(new ProblemElementRenderer());
		problemsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		problemsList.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
		problemsList.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				setVisible(false);
				ProblemElement temp = (ProblemElement) problemsList.getSelectedValue();
				int pType = temp.getProblemType();
				int pSubType = temp.getProblemSubType();
				getRelatedPanel(pType, pSubType, temp.getRelatedStationKey(), temp.getRelatedClassKey());
				problems.removeAllElements();
				problemsList.removeAll();
				mc.checkModel();
				initializeList();
				problemsList.setListData(problems);
				if (isToJMVAConversion) {
					if (mc.isErrorFreeToJMVA()) {
						continueButton.setEnabled(true);
					}
					//else continueButton.setEnabled(false);
					if (((pType == ModelChecker.ERROR_PROBLEM) 
							&& ((pSubType == ModelChecker.OPEN_CLASS_BUT_NO_SOURCE_ERROR)))
							|| ((pType == ModelChecker.ERROR_PROBLEM) 
									&& ((pSubType == ModelChecker.NO_STATION_ERROR)))) {
						setVisible(false);
					} else {
						if (!mc.isEverythingOkToJMVA()) {
							show();
						}
					}
				} else {
					if (mc.isErrorFreeNormal()) {
						continueButton.setEnabled(true);
					}
					if ((pType == ModelChecker.ERROR_PROBLEM)
							&& ((pSubType == ModelChecker.NO_CLASSES_ERROR) 
									|| (pSubType == ModelChecker.SIMULATION_ERROR)
									|| (pSubType == ModelChecker.REFERENCE_STATION_ERROR)
									|| (pSubType == ModelChecker.SOURCE_WITH_NO_OPEN_CLASSES_ERROR) 
									|| (pSubType == ModelChecker.ROUTING_ERROR) 
									|| (pSubType == ModelChecker.SINK_BUT_NO_OPEN_CLASSES_ERROR))) {
						if (!mc.isEverythingOkNormal()) {
							show();
						}
					} else {
						setVisible(false);
					}
				}

			}

		});

		JPanel containerPanel = new JPanel();
		containerPanel.setLayout(new BorderLayout());
		JPanel blankPanel = new JPanel();
		blankPanel.setBackground(Color.WHITE);
		containerPanel.add(problemsList, BorderLayout.NORTH);
		containerPanel.add(blankPanel, BorderLayout.CENTER);

		JScrollPane jsp = new JScrollPane(containerPanel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
				ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		jsp.setPreferredSize(new Dimension(310, 230));
		gbconstants.insets.top = 10;
		this.addComponent(title, gblayout, gbconstants, 0, 0, 2, 1);
		gbconstants.insets.top = 20;
		gbconstants.insets.left = -38;
		typeButton = new JButton("Type");
		typeButton.setPreferredSize(new Dimension(100, 15));
		this.addComponent(typeButton, gblayout, gbconstants, 1, 0, 1, 1);
		descriptionButton = new JButton("Description");
		descriptionButton.setPreferredSize(new Dimension(333, 15));
		gbconstants.insets.left = -68;
		this.addComponent(descriptionButton, gblayout, gbconstants, 1, 1, 1, 1);
		gbconstants.fill = GridBagConstraints.BOTH;
		gbconstants.insets.top = 0;
		gbconstants.weightx = 1;
		gbconstants.weighty = 1;
		gbconstants.insets.right = 10;
		gbconstants.insets.left = 10;
		this.addComponent(jsp, gblayout, gbconstants, 2, 0, 2, 1);
		ButtonEventHandler beh = new ButtonEventHandler();
		continueButton = new JButton("Continue");
		continueButton.setPreferredSize(new Dimension(80, 25));
		continueButton.addActionListener(beh);
		cancelButton = new JButton("Cancel");
		cancelButton.setPreferredSize(new Dimension(80, 25));
		cancelButton.setSelected(true);
		cancelButton.addActionListener(beh);
		if (isToJMVAConversion) {
			if (!mc.isErrorFreeToJMVA()) {
				continueButton.setEnabled(false);
			}
		} else {
			if (!mc.isErrorFreeNormal()) {
				continueButton.setEnabled(false);
			}
		}
		gbconstants.fill = GridBagConstraints.NONE;
		gbconstants.insets.left = 50;
		this.addComponent(continueButton, gblayout, gbconstants, 3, 0, 1, 1);
		gbconstants.insets.right = -45;
		this.addComponent(cancelButton, gblayout, gbconstants, 3, 1, 1, 1);
		this.setSize(450, 435);
		this.setLocation(300, 190);
		this.setModal(true);
		this.setResizable(false);
		this.setFocusableWindowState(false);
	}

	/**
	 * create the ProblemElements and insert them into the problems vector
	 */
	private void initializeList() {
		if (isToJMVAConversion) {
			if (mc.isThereNoClassesError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.NO_CLASSES_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>No classes defined", null, null));
			}
			if (mc.isThereNoStationError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.NO_STATION_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>No station defined", null, null));
			}
			if (mc.isThereOpenClassButNoSourceError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.OPEN_CLASS_BUT_NO_SOURCE_ERROR,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>Open class found but no source defined",
								null, null));
			}
			if (mc.isThereClassesWithoutRefStationError()) {
				Vector temp = mc.getKeysOfClassesWithoutRefStation();
				for (int i = 0; i < temp.size(); i++) {
					Object classKey = temp.get(i);
					String className = mc.getClassModel().getClassName(classKey);
					problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.REFERENCE_STATION_ERROR,
							"<html><font color=\"white\">----</font><b>Error</b>" +
							"<font color=\"white\">---------</font>No reference station defined for "
									+ className, null, classKey));
				}
			}
			/*if (mc.isThereOpenClassReferenceStationError()) {
			    Vector openClasses = mc.getKeysOfOpenClassesWithoutRefStation();
			    for (int i=0; i<openClasses.size(); i++) {
			        String className = mc.getClassModel().getClassName(openClasses.get(i));
			        problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM,ModelChecker.OPEN_CLASS_REFERENCE_STATION_ERROR,"<html><font color=\"white\">----</font><b>Error</b><font color=\"white\">---------</font>Open class " + className + " has not a reference station",null,null));
			    }
			}*/

			/*if (mc.isThereNoExpFoundWarning()) {
			    problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM,ModelChecker.NO_EXP_FOUND_WARNING,"<html><font color=\"white\">--</font><i>Warning</i><font color=\"white\">--------</font>A non-exponential time distribution was found",null,null));
			}
			/*if (mc.isThereDelaysFoundError()) {
			    problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM,ModelChecker.DELAYS_FOUND_ERROR,"      Error             Delays not supported in JModel to JMVA conversion",null,null));
			}*/
			/*if (mc.isThereDifferentServiceTimeWarning()) {
			    problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM,ModelChecker.DIFFERENT_SERVICE_TIME_WARNING,"<html><font color=\"white\">--</font><i>Warning</i><font color=\"white\">--------</font>A station with different mean service time per class was found",null,null));
			}
			if (mc.isThereNonFCFSWarning()) {
			    problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM,ModelChecker.NON_FCFS_WARNING,"<html><font color=\"white\">--</font><i>Warning</i><font color=\"white\">--------</font>A non FCFS queue strategy was found",null,null));
			}*/
			if (mc.isThereBCMPDifferentQueueingStrategyWarning()) {
				Vector temp = mc.getBCMPserversWithDifferentQueueStrategy();
				for (int i = 0; i < temp.size(); i++) {
					String thisStation = mc.getStationModel().getStationName(temp.get(i));
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.BCMP_DIFFERENT_QUEUEING_STRATEGIES_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i>" +
							"<font color=\"white\">--------</font>Different per class queueing strategy found at "
									+ thisStation, temp.get(i), null));
				}
			}
			if (mc.isThereBCMPDifferentServiceTypeWarning()) {
				Vector temp = mc.getBCMPserversWithDifferentServiceTypes();
				for (int i = 0; i < temp.size(); i++) {
					String thisStation = mc.getStationModel().getStationName(temp.get(i));
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TYPES_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i>" +
							"<font color=\"white\">--------</font>Non uniform service strategy inside FCFS station "
									+ thisStation, temp.get(i), null));
				}
			}
			if (mc.isThereBCMPFcfsNonExponentialWarning()) {
				Vector temp = mc.getBCMPserversFCFSWithoutExponential();
				for (int i = 0; i < temp.size(); i++) {
					String thisStation = mc.getStationModel().getStationName(temp.get(i));
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.BCMP_FCFS_EXPONENTIAL_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i>" +
							"<font color=\"white\">--------</font>Non exponential service time inside FCFS station "
									+ thisStation, temp.get(i), null));
				}
			}
			if (mc.isThereBCMPFcfsDifferentServiceTimesWarning()) {
				Vector temp = mc.getBCMPFcfsServersWithDifferentServiceTimes();
				for (int i = 0; i < temp.size(); i++) {
					String thisStation = mc.getStationModel().getStationName(temp.get(i));
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.BCMP_FCFS_DIFFERENT_SERVICE_TIMES_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i>" +
							"<font color=\"white\">--------</font>Different service times inside FCFS station "
									+ thisStation, temp.get(i), null));
				}
			}
			if (mc.isThereBCMPDelayWarning()) {
				Vector temp = mc.getBCMPdelaysWithNonRationalServiceDistribution();
				for (int i = 0; i < temp.size(); i++) {
					String thisStation = mc.getStationModel().getStationName(temp.get(i));
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.BCMP_FCFS_EXPONENTIAL_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i><font color=\"white\">--------</font>" + thisStation
									+ " with non valid service time distribution", temp.get(i), null));
				}
			}
			if (mc.isThereBCMPNonStateIndependentRoutingWarning()) {
				problems
						.add(new ProblemElement(
								ModelChecker.WARNING_PROBLEM,
								ModelChecker.BCMP_NON_STATE_INDEPENDENT_ROUTING_WARNING,
								"<html><font color=\"white\">--</font><i>Warning</i>" +
								"<font color=\"white\">--------</font>A non state independent routing strategy was found",
								null, null));
			}
			//TODO: Lcfs case handling
			//TODO: Processor Sharing handling
		} else {
			if (mc.isThereNoClassesError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.NO_CLASSES_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>No classes defined", null, null));
			}
			if (mc.isThereNoStationError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.NO_STATION_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>No station defined", null, null));
			}
			if (mc.isThereStationLinkError()) {
				Vector temp = mc.getKeysOfStationsWithLinkProblems();
				for (int i = 0; i < temp.size(); i++) {
					Object stationKey = temp.get(i);
					String stationName = mc.getStationModel().getStationName(stationKey);
					String description;
					if (mc.getStationModel().getStationType(stationKey).equals(CommonConstants.STATION_TYPE_SINK)) {
						description = ("<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>" + stationName + " has no ingoing links! No routing is possible");
					} else {
						description = ("<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>" + stationName + "  has no outgoing links! No routing is possible");
					}
					problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.STATION_LINK_ERROR, description, stationKey, null));
				}
			}
			if (mc.isThereAllForwardStationsAreSinkErrors()) {
				HashMap temp = mc.getKeysOfAllForwardStationsAreSinkErrors();
				Vector classKeys = mc.getClassModel().getClassKeys();
				for (int i = 0; i < classKeys.size(); i++) {
					Object classKey = classKeys.get(i);
					String className = mc.getClassModel().getClassName(classKey);
					Vector stationWithAllForwardStationsAreSinkErrors = (Vector) temp.get(classKey);
					if (stationWithAllForwardStationsAreSinkErrors != null) {
						for (int j = 0; j < stationWithAllForwardStationsAreSinkErrors.size(); j++) {
							Object stationKey = stationWithAllForwardStationsAreSinkErrors.get(j);
							String stationName = mc.getStationModel().getStationName(stationKey);
							problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.ALL_FORWARD_STATION_ARE_SINK_ERROR,
									"<html><font color=\"white\">----</font><b>Error</b>" +
									"<font color=\"white\">---------</font>Close class "
											+ className + " routed to station " + stationName + " linked only to sink", stationKey, classKey));
						}
					}
				}
			}
			if (mc.isThereRoutingError()) {
				HashMap temp = mc.getKeysOfRoutingProblems();
				Vector classKeys = mc.getClassModel().getClassKeys();
				for (int i = 0; i < classKeys.size(); i++) {
					Object classKey = classKeys.get(i);
					String className = mc.getClassModel().getClassName(classKey);
					Vector stationWithRoutingProblems = (Vector) temp.get(classKey);
					if (stationWithRoutingProblems != null) {
						for (int j = 0; j < stationWithRoutingProblems.size(); j++) {
							Object stationKey = stationWithRoutingProblems.get(j);
							String stationName = mc.getStationModel().getStationName(stationKey);
							problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.ROUTING_ERROR,
									"<html><font color=\"white\">----</font><b>Error</b>" +
									"<font color=\"white\">---------</font>Close class "
											+ className + " at station " + stationName + " is routed to sink with p=1", stationKey, classKey));
						}
					}
				}
			}
			if (mc.isThereSimulationError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.SIMULATION_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>No performance indices defined",
						null, null));
			}
			if (mc.isThereClassesWithoutRefStationError()) {
				Vector temp = mc.getKeysOfClassesWithoutRefStation();
				for (int i = 0; i < temp.size(); i++) {
					Object classKey = temp.get(i);
					String className = mc.getClassModel().getClassName(classKey);
					problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.REFERENCE_STATION_ERROR,
							"<html><font color=\"white\">----</font><b>Error</b>" +
							"<font color=\"white\">---------</font>No reference station defined for "
									+ className, null, classKey));
				}
			}
			if (mc.isThereNoSinkWithOpenClassesError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.NO_SINK_WITH_OPEN_CLASSES_ERROR,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>Open classes were found but no sink have been defined",
								null, null));
			}
			if (mc.isThereSinkButNoOpenClassError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.SINK_BUT_NO_OPEN_CLASSES_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>Sink without open classes", null,
						null));
			}
			if (mc.isThereOpenClassButNoSourceError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.OPEN_CLASS_BUT_NO_SOURCE_ERROR,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>An open class was found but no source has been defined",
								null, null));
			}
			if (mc.isThereSourceWithNoClassesError()) {
				Vector temp = mc.getKeysOfSourceWithoutClasses();
				for (int i = 0; i < temp.size(); i++) {
					Object sourceKey = temp.get(i);
					String sourceName = mc.getStationModel().getStationName(sourceKey);
					problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.SOURCE_WITH_NO_OPEN_CLASSES_ERROR,
							"<html><font color=\"white\">----</font><b>Error</b>" +
							"<font color=\"white\">---------</font>" + sourceName
									+ " without open classes associated", sourceKey, null));
				}
			}
			if (mc.isThereSinkPerfIndicesWithNoSinkError()){
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.SINK_PERF_IND_WITH_NO_SINK_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>" +
						"Response Time per Sink and Throughput per sink should not be used " +
						"if there is no Sink defined in the model.",
						null, null));
			}			
			if (mc.isThereInconsistentMeasureError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.INCONSISTENT_MEASURE_ERROR,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>Undefined station in performance index",
								null, null));
			}
			if (mc.isThereMeasureError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.DUPLICATE_MEASURE_ERROR,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>A performance index is defined more than once",
								null, null));
			}
			if (mc.isTherejoinWithoutForkError()) {
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.JOIN_WITHOUT_FORK_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>Join without fork", null, null));
			}
			if (mc.isThereEmptyBlockingRegionError()) {
				Vector regionKeys = mc.getKeysOfEmptyBlockingRegions();
				for (int i = 0; i < regionKeys.size(); i++) {
					String name = mc.getBlockingModel().getRegionName(regionKeys.get(i));
					problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.EMPTY_BLOCKING_REGION,
							"<html><font color=\"white\">----</font><b>Error</b>" +
							"<font color=\"white\">---------</font>Finite Capacity Region " + name
									+ " is empty", regionKeys.get(i), null));
				}
			}
			if (mc.isTherePreloadingInBlockingRegionError()) {
				problems
						.add(new ProblemElement(
								ModelChecker.ERROR_PROBLEM,
								ModelChecker.PRELOADING_WITH_BLOCKING,
								"<html><font color=\"white\">----</font><b>Error</b>" +
								"<font color=\"white\">---------</font>Preloading of stations inside a blocking region is not supported",
								null, null));
			}
			if (mc.isThereMoreThanOneSinkWarning()) {
				problems
						.add(new ProblemElement(
								ModelChecker.WARNING_PROBLEM,
								ModelChecker.MORE_THAN_ONE_SINK_WARNING,
								"<html><font color=\"white\">--</font><i>Warning</i>" +
								"<font color=\"white\">--------</font>More than one sink defined, measures may not be accurate",
								null, null));
			}
			if (mc.isThereNoBackwardLinkWarning()) {
				Vector temp = mc.getKeysOfStationWithoutBackwardLinks();
				for (int i = 0; i < temp.size(); i++) {
					Object stationKey = temp.get(i);
					String stationName = mc.getStationModel().getStationName(stationKey);
					problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.NO_BACKWARD_LINK_WARNING,
							"<html><font color=\"white\">--</font><i>Warning</i>" +
							"<font color=\"white\">--------</font>" + stationName
									+ " is not backward linked", stationKey, null));
				}
			}
			if (mc.isThereParametricAnalysisModelModifiedWarning()) {
				problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.PARAMETRIC_ANALYSIS_MODEL_MODIFIED_WARNING,
						"<html><font color=\"white\">--</font><i>Warning</i>" +
						"<font color=\"white\">--------</font>What-if analysis model modified",
						null, null));
			}
			if (mc.isThereParametricAnalysisNoMoreAvaibleWarning()) {
				problems.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.PARAMETRIC_ANALYSIS_NO_MORE_AVAIBLE_WARNING,
						"<html><font color=\"white\">--</font><i>Warning</i>" +
						"<font color=\"white\">--------</font>What-if analysis not avaible", null,
						null));
			}
			if (mc.isThereForkWithoutJoinWarnings()) {
				problems
						.add(new ProblemElement(ModelChecker.WARNING_PROBLEM, ModelChecker.FORK_WITHOUT_JOIN_WARNING,
								"<html><font color=\"white\">--</font><i>Warning</i>" +
								"<font color=\"white\">--------</font>Fork found but no join",
								null, null));
			}
			if (mc.isSinkPerfIndicesWithClosedClassError()){
				problems.add(new ProblemElement(ModelChecker.ERROR_PROBLEM, ModelChecker.SINK_PERF_WITH_CLOSED_CLASS_ERROR,
						"<html><font color=\"white\">----</font><b>Error</b>" +
						"<font color=\"white\">---------</font>" +
						"Response Time per Sink and Throughput per sink should not be used for closed class.",
						null, null));
			}
			if (mc.isThereSinkProbabilityUpdateWarning()) {
				problems
						.add(new ProblemElement(
								ModelChecker.WARNING_PROBLEM,
								ModelChecker.SINK_PROBABILITY_UPDATE_WARNING,
								"<html><font color=\"white\">--</font><i>Warning</i>" +
								"<font color=\"white\">--------</font>Sink Probability of the Closed Class(s) " +
								" "+ mc.getClassModel().getsinkProbabilityUpdateClasses() + " " +
								"of Station(s)" +
								" " + mc.getStationModel().getsinkProbabilityUpdateStations() + " " +
								"has been updated to 0.0.",
								null, null));
				//Reset after displaying the offending Station and Class names.
				mc.getClassModel().resetSinkProbabilityUpdateClasses();
				mc.getStationModel().resetSinkProbabilityUpdateStations();
			}
		}
	}

	/**
	 * Enable or not to run simulation/conversion to JMVA
	 * @return true if the simulation is runnable and user wants to run simulation
	 */
	public boolean continued() {
		if (isToJMVAConversion) {
			if ((!operationCanceled) && (mc.isErrorFreeToJMVA())) {
				canBeRun = true;
			}
		} else {
			if ((!operationCanceled) && (mc.isErrorFreeNormal())) {
				canBeRun = true;
			}
		}
		return canBeRun;
	}

	private void addComponent(Component component, GridBagLayout gbl, GridBagConstraints gbc, int row, int column, int width, int heigth) {
		Container c = this.getContentPane();

		gbc.gridx = column;
		gbc.gridy = row;

		gbc.gridwidth = width;
		gbc.gridheight = heigth;

		gbl.setConstraints(component, gbc);
		c.add(component);
	}

	private void getRelatedPanel(int problemType, int problemSubType, Object relatedStation, Object relatedClass) {
		gi.showRelatedPanel(problemType, problemSubType, relatedStation, relatedClass);
	}

	private class ProblemElementRenderer implements ListCellRenderer {
		private String[] iconNames = new String[] { "Error", "Warning" };
		private Icon[] icons = new Icon[iconNames.length];
		private int[] problemTypes = { ModelChecker.ERROR_PROBLEM, ModelChecker.WARNING_PROBLEM };

		public ProblemElementRenderer() {
			for (int i = 0; i < iconNames.length; i++) {
				icons[i] = JMTImageLoader.loadImage(iconNames[i], new Dimension(16, 16));
			}
		}

		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
			JLabel label = null;
			for (int i = 0; i < problemTypes.length; i++) {
				if (problemTypes[i] == ((ProblemElement) value).getProblemType()) {
					String errorDescription = ((ProblemElement) value).getDescription();
					label = new JLabel(errorDescription, icons[i], SwingConstants.LEFT);
				}
			}
			if (label == null) {
				String errorDescription;
				if (((Integer) value).intValue() == 0) {
					errorDescription = "Error";
				} else {
					errorDescription = "Warning";
				}
				label = new JLabel(errorDescription);
			}
			label.setOpaque(true);
			label.setBorder(new LineBorder(cellHasFocus ? Color.BLUE : Color.WHITE));
			label.setBackground(isSelected ? list.getSelectionBackground() : Color.WHITE);
			label.setForeground(isSelected ? list.getSelectionForeground() : Color.BLACK);
			label.setFont(isSelected ? label.getFont().deriveFont(Font.BOLD) : label.getFont().deriveFont(Font.ROMAN_BASELINE));

			return label;
		}
	}

	private class ProblemElement {
		int type;
		int subType;
		String description;
		Object relatedStationKey;
		Object relatedClassKey;

		public ProblemElement(int type, int subType, String description, Object relatedStationKey, Object relatedClassKey) {
			this.type = type;
			this.subType = subType;
			this.description = description;
			this.relatedStationKey = relatedStationKey;
			this.relatedClassKey = relatedClassKey;
		}

		public int getProblemType() {
			return type;
		}

		public int getProblemSubType() {
			return subType;
		}

		public String getDescription() {
			return description;
		}

		public Object getRelatedStationKey() {
			return relatedStationKey;
		}

		public Object getRelatedClassKey() {
			return relatedClassKey;
		}
	}

	private class ButtonEventHandler implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (e.getSource() == cancelButton) {
				operationCanceled = true;
				dispose();
			}

			else if (e.getSource() == continueButton) {
				operationCanceled = false;
				dispose();
			}
		}

	}

}
