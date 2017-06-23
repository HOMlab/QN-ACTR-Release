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
package jmt.gui.jwat.trafficAnalysis.panels;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.SpinnerDateModel;
import javax.swing.SpinnerListModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jmt.engine.jwat.VariableNumber;
import jmt.engine.jwat.trafficAnalysis.BurstEngine;
import jmt.engine.jwat.trafficAnalysis.ModelTrafficAnalysis;
import jmt.engine.jwat.trafficAnalysis.OnResetModel;
import jmt.engine.jwat.trafficAnalysis.OnSetParamtersListener;
import jmt.engine.jwat.trafficAnalysis.TrafficAnalysisSession;
import jmt.framework.gui.wizard.WizardPanel;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.MainJwatWizard;
import jmt.gui.jwat.trafficAnalysis.utils.NewPlot;

/**
 * <p>Title: Graph Panel</p>
 * <p>Description: This panelis used to display JMVA what-if analysis
 * results in a graph. Number of allowed lines in graph is determined
 * by <code>graph.getColors().length</code>. Modify it to allow more lines.</p>
 *
 * @author Bertoli Marco
 *         Date: 1-giu-2006
 *         Time: 11.01.29
 */
public class GraphArrivalPanel extends WizardPanel implements JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public static final String[] INDICES_TYPES = { "Years", "Months", "Days", "Hours", "Minutes" };
	public static final String DESCRIPTION_GRAPH = "<html><body align=\"left\"><font size=\"4\"><b>Arrival values</b>"
			+ "</font><font size=\"3\"><br>Select granularity to plot the arrival rate. Left-click and drag on the graph to zoom "
			+ "it, right-click to save it in EPS or PNG format.</body></html>";

	// Plot
	private NewPlot graph;
	// Performance index selector
	private JComboBox index;
	// Bounds for graph
	private JSpinner Xmin, Xmax, Ymin, Ymax;
	// Tells if spinner update is forced. This is needed to avoid that updates made by
	// code will be interpreted as updated made by user.
	private boolean forcedUpdate = false;
	// Table used to select performance indices to be plotted
	private JTable table;
	// Scrollpane used for table
	private JScrollPane tableScrollPane;
	// Dimension of bounds spinners
	final static Dimension DIM_SPINNER = new Dimension(95, 20);
	// Current performance index
	private String currentIndex = "Years";
	private MainJwatWizard ew;
	private Vector columnHeads;
	private Vector<Vector<Comparable>> rows;
	private JPanel left, mainPanel;
	private double[] res;
	private boolean minute = false, month = false, day = false, hour = false, year = false;
	private long nYear, nMonth, nDay, nHour, nMinute = 0;
	private VariableNumber v;
	private SpinnerDateModel Sp1, Sp2;
	Vector<double[]> ArrDay, ArrYear, ArrMonth, ArrHour, ArrMinute;
	private BurstEngine engine = null;

	/**
	 * Builds a new GraphPanel, given an exact model data structure
	 * @param model reference to data structure
	 */
	public GraphArrivalPanel(MainJwatWizard ew) {
		this.ew = ew;
		((TrafficAnalysisSession) ew.getSession()).addSetParamsListener(new OnSetParamtersListener() {
			public void ParamsSetted() {
				System.err.println("GraphArrival Panel reset");
				GraphArrivalPanel.this.removeAll();
				engine = ((TrafficAnalysisSession) GraphArrivalPanel.this.ew.getSession()).getEngine();
				initGraphics();
				calcGran();
				paintIndex(1);
			}
		});
		((ModelTrafficAnalysis) ew.getModel()).addResetModelListener(new OnResetModel() {
			public void modelResetted() {
				GraphArrivalPanel.this.removeAll();
			}
		});
	}

	/**
	 * Initialize GUI of this panel
	 */
	private void initGraphics() {
		setLayout(new BorderLayout(10, 10));
		setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
		mainPanel = new JPanel(new BorderLayout(5, 5));
		mainPanel.setBorder(BorderFactory.createEtchedBorder());

		// Adds description label
		JLabel descrLabel = new JLabel(DESCRIPTION_GRAPH);
		add(descrLabel, BorderLayout.NORTH);
		add(mainPanel, BorderLayout.CENTER);

		// Creates left panel with options
		left = new JPanel(new BorderLayout(3, 3));
		// Adds performance index selection
		JPanel indexPanel = new JPanel();
		JLabel pIndex = new JLabel("Granularity: ");
		index = new JComboBox(INDICES_TYPES);
		pIndex.setLabelFor(index);
		indexPanel.add(pIndex);
		indexPanel.add(index);
		left.add(indexPanel, BorderLayout.NORTH);
		// Adds panel for bounds selection
		JPanel boundsPanel = new JPanel();
		Box b = Box.createHorizontalBox();
		b.add(Box.createHorizontalStrut(5));
		b.add(new JLabel("Xmin: ", SwingConstants.RIGHT));
		Xmin = new JSpinner();
		Xmin.setPreferredSize(DIM_SPINNER);
		b.add(Xmin);
		b.add(Box.createHorizontalStrut(5));
		b.add(new JLabel("Xmax: ", SwingConstants.RIGHT));
		Xmax = new JSpinner();
		Xmax.setPreferredSize(DIM_SPINNER);
		b.add(Xmax);
		Box b1 = Box.createHorizontalBox();
		b1.add(Box.createHorizontalStrut(5));
		b1.add(new JLabel("Ymin: ", SwingConstants.RIGHT));
		Ymin = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymin.setPreferredSize(DIM_SPINNER);
		b1.add(Ymin);
		b1.add(Box.createHorizontalStrut(5));
		b1.add(new JLabel("Ymax: ", SwingConstants.RIGHT));
		Ymax = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1e10, 0.01));
		Ymax.setPreferredSize(DIM_SPINNER);
		b1.add(Ymax);
		Box vb = Box.createVerticalBox();
		vb.add(b);
		vb.add(b1);
		left.add(vb, BorderLayout.SOUTH);

		mainPanel.add(left, BorderLayout.WEST);

		// Puts graph in the right panel
		// Creates label for X-axis
		String xLabel = "";
		graph = new NewPlot();
		xLabel = "Time";
		graph.setXLabel(currentIndex);
		graph.setXRange(0, 0);
		graph.setYLabel("Job / s");
		mainPanel.add(graph, BorderLayout.CENTER);
		columnHeads = new Vector();
		columnHeads.add(" ");
		columnHeads.add("Time");
		columnHeads.add("Job / s");
		table = new JTable(rows, columnHeads);

		tableScrollPane = new JScrollPane(table);
		tableScrollPane.setPreferredSize(new Dimension(160, tableScrollPane.getPreferredSize().height));
		left.add(tableScrollPane, BorderLayout.CENTER);
		updateSpinners();
		addActions();

	}

	public void calcGran() {
		v = ((ModelTrafficAnalysis) ew.getModel()).getMatrix().getVariables()[0];
		currentIndex = checkGranularity(v);
		index.setSelectedItem(currentIndex);
	}

	/**
	 * Is the method invoked to set the values in the table
	 */
	public void setTable(int j) {
		rows = new Vector<Vector<Comparable>>();
		DecimalFormat df = new DecimalFormat("0.00000000");
		int f = 0;
		Vector<Comparable> line;
		Date d = null;
		double[] temp;
		List<String> l = new Vector<String>();

		String s;

		switch (j) {
			case (0):
				res = new double[ArrYear.size()];
				//ew.getTAP().redraw(ArrYear,0);
				for (int i = 0; i < ArrYear.size(); i++) {

					line = new Vector<Comparable>();
					temp = ArrYear.get(i);
					d = new Date((long) temp[0]);
					line.add(new Integer(f));
					f++;
					s = new String("" + d.getYear());
					line.add(s);
					l.add(s);
					line.add(new Double(df.format(temp[1] / 31104000)));
					res[i] = temp[1] / 31104000;
					rows.add(line);
				}
				temp = ArrYear.get(0);
				d = new Date((long) temp[0]);
				s = new String("" + d.getYear());
				graph.setXLabel(currentIndex + " from " + s);
				break;
			case (1):
				res = new double[ArrMonth.size()];
				//ew.getTAP().redraw(ArrMonth,1);
				for (int i = 0; i < ArrMonth.size(); i++) {

					line = new Vector<Comparable>();
					temp = ArrMonth.get(i);
					d = new Date((long) temp[0]);
					line.add(new Integer(f));
					f++;
					s = new String(d.getMonth() + "/" + d.getYear());
					line.add(s);
					l.add(s);
					line.add(new Double(df.format(temp[1] / 2592000)));
					res[i] = temp[1] / 2592000;
					rows.add(line);
				}
				temp = ArrMonth.get(0);
				d = new Date((long) temp[0]);
				s = new String(d.getMonth() + "/" + d.getYear());
				graph.setXLabel(currentIndex + " from " + s);
				break;
			case (2):
				res = new double[ArrDay.size()];
				//ew.getTAP().redraw(ArrDay,2);
				for (int i = 0; i < ArrDay.size(); i++) {

					line = new Vector<Comparable>();
					temp = ArrDay.get(i);
					d = new Date((long) temp[0]);
					line.add(new Integer(f));
					f++;
					s = new String(d.getDate() + "/" + d.getMonth() + "/" + d.getYear());
					line.add(s);
					l.add(s);
					line.add(new Double(df.format(temp[1] / 86400)));
					res[i] = temp[1] / 86400;
					rows.add(line);
				}
				temp = ArrDay.get(0);
				d = new Date((long) temp[0]);
				s = new String(d.getDate() + "/" + d.getMonth() + "/" + d.getYear());
				graph.setXLabel(currentIndex + " from " + s);
				break;
			case (3):

				res = new double[ArrHour.size()];
				//ew.getTAP().redraw(ArrHour,3);	
				for (int i = 0; i < ArrHour.size(); i++) {

					line = new Vector<Comparable>();
					temp = ArrHour.get(i);
					d = new Date((long) temp[0]);
					line.add(new Integer(f));
					f++;

					SimpleDateFormat dfo = new SimpleDateFormat("dd/MM/yyyy HH.00");
					s = dfo.format(d);
					//s=new String(d.getDate()+"/"+d.getMonth()+"/"+d.getYear()+" "+d.getHours()+".00");

					line.add(s);
					l.add(s);
					line.add(new Double(df.format(temp[1] / 3600)));
					res[i] = temp[1] / 3600;
					rows.add(line);
				}
				temp = ArrHour.get(0);
				d = new Date((long) temp[0]);

				SimpleDateFormat dfo = new SimpleDateFormat("dd/MM/yyyy HH.00");
				s = dfo.format(d);
				//s=new String(d.getDate()+"/"+d.getMonth()+"/"+d.getYear()+" "+d.getHours()+".00");

				graph.setXLabel(currentIndex + " from " + s);
				break;
			case (4):

				res = new double[ArrMinute.size()];
				//ew.getTAP().redraw(ArrMinute,4);
				for (int i = 0; i < ArrMinute.size(); i++) {

					line = new Vector<Comparable>();
					temp = ArrMinute.get(i);
					d = new Date((long) temp[0]);
					line.add(new Integer(f));
					f++;
					SimpleDateFormat dfoM = new SimpleDateFormat("dd/MM/yyyy HH:mm");
					//s=new String(d.getDate()+"/"+d.getMonth()+"/"+d.getYear()+" "+d.getHours()+"."+d.getMinutes());
					s = dfoM.format(d);
					line.add(s);
					l.add(s);
					line.add(new Double(df.format(temp[1] / 60)));
					res[i] = temp[1] / 60;
					rows.add(line);
				}
				temp = ArrMinute.get(0);
				d = new Date((long) temp[0]);

				SimpleDateFormat dfoM = new SimpleDateFormat("dd/MM/yyyy HH:mm");
				s = dfoM.format(d);

				s = new String(d.getDate() + "/" + d.getMonth() + "/" + d.getYear() + " " + d.getHours() + "." + d.getMinutes());
				graph.setXLabel(currentIndex + " from " + s);
				break;

		}

		Xmin.setModel(new SpinnerListModel(l));
		Xmax.setModel(new SpinnerListModel(l));
		Xmax.setValue(l.get(l.size() - 1));
		left.remove(tableScrollPane);

		table = new JTable(rows, columnHeads);
		table.setEnabled(false);
		table.getColumnModel().getColumn(0).setMaxWidth(30);
		table.getColumnModel().getColumn(1).setPreferredWidth(65);
		table.getColumnModel().getColumn(2).setPreferredWidth(65);
		table.setRowHeight(18);
		tableScrollPane = new JScrollPane(table);
		tableScrollPane.setPreferredSize(new Dimension(160, tableScrollPane.getPreferredSize().height));
		left.add(tableScrollPane, BorderLayout.CENTER);

		mainPanel.updateUI();

	}

	/**
	 * Is the method invoked to calculate the number of minutes in the log (Variable)
	 */
	private void ArrNumMinute() {
		ArrMinute = new Vector<double[]>();
		double i[];
		Calendar cal = Calendar.getInstance();
		cal.setTime(new Date((long) v.getValue(0)));
		Calendar cal2 = Calendar.getInstance();
		int y = 0;
		boolean k = false;
		int z = 0;
		cal2.setTimeInMillis((long) v.getValue(z));
		while (z < v.Size()) {

			if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
					&& cal.get(Calendar.DATE) == cal2.get(Calendar.DATE) && cal.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY)
					&& cal.get(Calendar.MINUTE) == cal2.get(Calendar.MINUTE)) {
				y++;
			} else {
				k = false;
				i = new double[2];
				i[0] = cal.getTimeInMillis();
				i[1] = y;
				ArrMinute.addElement(i);
				if (cal.get(Calendar.MINUTE) == 59 && cal.get(Calendar.HOUR_OF_DAY) != 23) {
					cal.roll(Calendar.HOUR_OF_DAY, true);
				}
				if (cal.get(Calendar.MINUTE) == 59 && cal.get(Calendar.HOUR_OF_DAY) == 23) {
					cal.roll(Calendar.HOUR_OF_DAY, true);
					cal.roll(Calendar.DAY_OF_YEAR, true);
				}
				cal.roll(Calendar.MINUTE, true);
				if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
						&& cal.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)
						&& cal.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY) && cal.get(Calendar.MINUTE) == cal2.get(Calendar.MINUTE)) {
					y = 1;
				} else {
					y = 0;
					k = true;
				}
			}
			if (k != true) {
				z++;
				if (z < v.Size()) {
					cal2.setTimeInMillis((long) v.getValue(z));
				}
			}

		}
		i = new double[2];
		i[0] = cal.getTimeInMillis();
		i[1] = y;
		ArrMinute.addElement(i);

	}

	/**
	 * Is the method invoked to calculate the number of hours in the log (Variable)
	 */
	private void ArrNumHour() {
		ArrHour = new Vector<double[]>();
		double i[];
		Calendar cal = Calendar.getInstance();
		cal.setTime(new Date((long) v.getValue(0)));
		Calendar cal2 = Calendar.getInstance();
		int y = 0;
		int z = 0;
		boolean k = false;
		while (z < v.Size()) {
			cal2.setTimeInMillis((long) v.getValue(z));
			if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
					&& cal.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)
					&& cal.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY)) {
				y++;
			} else {
				k = false;
				i = new double[2];
				i[0] = cal.getTimeInMillis();
				i[1] = y;
				ArrHour.addElement(i);

				if (cal.get(Calendar.HOUR_OF_DAY) == 23) {
					cal.roll(Calendar.DAY_OF_YEAR, true);
				}
				cal.roll(Calendar.HOUR_OF_DAY, true);
				if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
						&& cal.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)
						&& cal.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY)) {
					y = 1;
				} else {
					y = 0;
					k = true;
				}
			}
			if (k != true) {
				z++;
			}

		}
		i = new double[2];
		i[0] = cal.getTimeInMillis();
		i[1] = y;
		ArrHour.addElement(i);

	}

	/**
	 * Is the method invoked to calculate the number of days in the log (Variable)
	 */
	private void ArrNumDay() {
		ArrDay = new Vector<double[]>();
		double i[];
		Calendar cal = Calendar.getInstance();
		cal.setTimeInMillis((long) v.getValue(0));
		Calendar cal2 = Calendar.getInstance();
		int y = 0;
		for (int z = 0; z < v.Size(); z++) {
			cal2.setTimeInMillis((long) v.getValue(z));
			if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
					&& cal.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) {
				y++;
			} else {
				i = new double[2];
				i[0] = cal.getTimeInMillis();
				i[1] = y;
				ArrDay.addElement(i);
				cal.roll(Calendar.DAY_OF_YEAR, true);
				if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)
						&& cal.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) {
					y = 1;
				} else {
					y = 0;
				}
			}

		}
		i = new double[2];
		i[0] = cal.getTimeInMillis();
		i[1] = y;
		ArrDay.addElement(i);

	}

	/**
	 * Is the method invoked to calculate the number of months in the log (Variable)
	 */
	private void ArrNumMonth() {
		ArrMonth = new Vector<double[]>();
		double i[];
		Calendar cal = Calendar.getInstance();
		Calendar cal2 = Calendar.getInstance();
		cal.setTimeInMillis((long) v.getValue(0));
		int y = 0;
		boolean k = false;
		int z = 0;
		while (z < v.Size()) {

			cal2.setTimeInMillis((long) v.getValue(z));
			if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)) {
				y++;
			} else {
				k = false;
				i = new double[2];
				i[0] = cal.getTimeInMillis();
				i[1] = y;
				ArrMonth.addElement(i);
				cal.roll(Calendar.MONTH, true);
				if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR) && cal.get(Calendar.MONTH) == cal2.get(Calendar.MONTH)) {
					y = 1;

				} else {
					y = 0;
					k = false;
				}
			}
			if (k != true) {
				z++;
			}
		}
		i = new double[2];
		i[0] = cal.getTimeInMillis();
		i[1] = y;
		ArrMonth.addElement(i);

	}

	/**
	 * Is the method invoked to calculate the number of years in the log (Variable)
	 */
	private void ArrNumYear() {
		ArrYear = new Vector<double[]>();
		double i[];
		Calendar cal = Calendar.getInstance();
		Calendar cal2 = Calendar.getInstance();
		cal.setTimeInMillis((long) v.getValue(0));
		int y = 0;
		int z = 0;
		boolean k = false;
		while (z < v.Size()) {
			cal2.setTimeInMillis((long) v.getValue(z));
			if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) {
				y++;
			} else {
				k = false;
				i = new double[2];
				i[0] = cal.getTimeInMillis();
				i[1] = y;
				ArrYear.addElement(i);
				cal.roll(Calendar.YEAR, true);
				if (cal.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) {
					y = 1;
				} else {
					y = 0;
					k = true;
				}
			}
			if (k != true) {
				z++;
			}
		}
		i = new double[2];
		i[0] = cal.getTimeInMillis();
		i[1] = y;
		ArrYear.addElement(i);
	}

	/**
	 * Is the method invoked to calculate the best graph to show at the beginning and to calculte the suitable granularity of time 
	 * @param v is the variable object
	 */

	private String checkGranularity(VariableNumber v) {
		String r = null;
		this.year = false;
		this.month = false;
		this.day = false;
		this.minute = false;
		this.hour = false;
		Calendar cal = Calendar.getInstance();
		Calendar cal2 = Calendar.getInstance();

		long temp = (long) v.getValue(v.Size() - 1) - (long) v.getValue(0);
		cal2.setTimeInMillis((long) v.getValue(v.Size() - 1));
		cal.setTimeInMillis((long) v.getValue(0));

		//cal2.setTimeInMillis(new Date("12/01/2006").getTime());
		//cal.setTimeInMillis(new Date("12/01/2005").getTime());
		long year = 1;
		long month = 1;
		//Se le prima e ultima data hanno mese o anno differente calcolo il numero di mesi e anni di differenza
		if (cal.get(Calendar.MONTH) != cal2.get(Calendar.MONTH) || cal.get(Calendar.YEAR) != cal2.get(Calendar.YEAR)) {
			// ?  Non funziona --> conta + anni di quelli esistenti e la funzione roll non incrementa l'anno !!!
			while (cal.get(Calendar.MONTH) != cal2.get(Calendar.MONTH) || cal.get(Calendar.YEAR) != cal2.get(Calendar.YEAR)) {
				if (cal.get(Calendar.MONTH) != cal2.get(Calendar.MONTH)) {
					month++;
				}
				if (cal.get(Calendar.YEAR) != cal2.get(Calendar.YEAR)) {
					year++;
				}
				cal.roll(Calendar.MONTH, true);
				//System.err.println(new Date(cal.getTimeInMillis()));
			}
		}
		long minute = temp / (1000 * 60);
		long hour = minute / 60;
		long day = hour / 24;

		if (year > 1 && year < 559) {
			this.year = true;
			ArrNumYear();
			nYear = year;
			//return "Years";	
		}

		if (month > 1 && month < 559) {
			this.month = true;
			ArrNumMonth();
			nMonth = month;
			//return "Months";
		}
		if (day > 1 && day < 559) {
			this.day = true;
			ArrNumDay();
			nDay = day;
			//return "Days";
		}
		if (hour > 1 && hour < 559) {
			this.hour = true;
			ArrNumHour();
			nHour = hour;
			//return "Hours";
		}
		if (minute > 1 && minute < 559) {
			this.minute = true;
			ArrNumMinute();
			nMinute = minute;
			//return "Minutes";
		}
		if (year > 1 && r == null) {
			return r = "Years";
		}
		if (month > 1 && r == null) {
			return r = "Months";
		}
		if (day > 1 && r == null) {
			return r = "Days";
		}
		if (hour > 1 && r == null) {
			return r = "Hours";
		}
		if (r == null) {
			return r = "Minutes";
		}
		return r;
	}

	/**
	 * Updates values in spinners used to select ranges to be shown in graph
	 */
	private void updateSpinners() {
		// Check for special value used if graph is empty
		if (graph.getXRange()[0] != Double.MAX_VALUE) {
			//Xmin.setValue(new Double(graph.getXRange()[0]));
			// Xmax.setValue(new Double(graph.getXRange()[1]));
			Ymin.setValue(new Double(graph.getYRange()[0]));
			Ymax.setValue(new Double(graph.getYRange()[1]));
		} else {
			//Xmin.setValue(new Double(0.0));
			//Xmax.setValue(new Double(0.0));
			Ymin.setValue(new Double(0.0));
			Ymax.setValue(new Double(0.0));
		}
	}

	/**
	 * Used when a spinne value is updated
	 */
	private void setBounds() {
		double xmin, xmax, ymin, ymax;
		Object val;
		String val1 = (String) Xmin.getValue();
		Vector v = (Vector) ((SpinnerListModel) Xmin.getModel()).getList();
		xmin = v.indexOf(val1) + 1;
		String val2 = (String) Xmax.getValue();
		Vector v2 = (Vector) ((SpinnerListModel) Xmax.getModel()).getList();
		xmax = v2.indexOf(val2) + 1;
		val = Ymin.getValue();
		if (val instanceof Number) {
			ymin = ((Number) val).doubleValue();
		} else {
			ymin = graph.getYRange()[0];
		}
		val = Ymax.getValue();
		if (val instanceof Number) {
			ymax = ((Number) val).doubleValue();
		} else {
			ymax = graph.getYRange()[1];
		}

		// Sets bounds
		graph.setXRange(xmin, xmax);
		graph.setYRange(ymin, ymax);
		graph.repaint();
	}

	/**
	 * This function must be called each time selected performance
	 * index changes
	 */
	private void updateIndex() {
		String current = (String) index.getSelectedItem();

		currentIndex = current;
		if (((ModelTrafficAnalysis) ew.getModel()).getMatrix() != null) {
			paintIndex(1);
			// Updates graph
		}
		// graph.setXLabel(current);
		graph.repaint();
	}

	/**
	 * Adds action listeners to GUI components
	 */
	private void addActions() {
		// Listener used for bounds spinners
		ChangeListener boundsListener = new ChangeListener() {
			public void stateChanged(ChangeEvent e) {
				if (!forcedUpdate) {
					setBounds();
					updateSpinners();
				}
			}
		};
		Xmin.addChangeListener(boundsListener);
		Xmax.addChangeListener(boundsListener);
		Ymin.addChangeListener(boundsListener);
		Ymax.addChangeListener(boundsListener);
		// Listener for index selection comboBox
		index.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent arg0) {
				if ((((String) ((JComboBox) arg0.getSource()).getSelectedItem()) == "Years" && !year
						|| ((String) ((JComboBox) arg0.getSource()).getSelectedItem()) == "Months" && !month
						|| ((String) ((JComboBox) arg0.getSource()).getSelectedItem()) == "Days" && !day
						|| ((String) ((JComboBox) arg0.getSource()).getSelectedItem()) == "Hours" && !hour || ((String) ((JComboBox) arg0.getSource())
						.getSelectedItem()) == "Minutes"
						&& !minute)
						&& ((ModelTrafficAnalysis) ew.getModel()).getMatrix() != null) {
					error();
					return;
				}
				updateIndex();

			}
		});

		// Adds a listener to the graph to detect zoom events
		graph.addRescaleListener(new NewPlot.RescaleListener() {
			public void Rescaled() {
				forcedUpdate = true;
				updateSpinners();
				forcedUpdate = false;
			}
		});
	}

	private void error() {
		JOptionPane.showMessageDialog(this, "The log file is too small or too big for this granuarity", "Error", JOptionPane.ERROR_MESSAGE);

	}

	/**
	 * Paints performance index at specified row
	 * @param rowNum row number of index to be painted
	 */
	public void paintIndex(int rowNum) {
		//Years 
		if (currentIndex.equals(INDICES_TYPES[0]) && year) {
			graph.clear(rowNum);
			setTable(0);
			double[] xAxis = new double[res.length];
			int z = 0;
			for (int i = 0; i < res.length; i++) {
				xAxis[i] = z;
				z++;
			}
			graph.setXAxis(xAxis);
			graph.draw(rowNum, res);
		}
		// Months
		if (currentIndex.equals(INDICES_TYPES[1]) && month) {
			graph.clear(rowNum);
			setTable(1);
			double[] xAxis = new double[res.length];
			int z = 0;
			for (int i = 0; i < res.length; i++) {
				xAxis[i] = z;
				z++;
			}
			graph.setXAxis(xAxis);
			graph.draw(rowNum, res);
		}
		//Days
		if (currentIndex.equals(INDICES_TYPES[2]) && day) {
			graph.clear(rowNum);
			setTable(2);
			double[] xAxis = new double[res.length];
			int z = 0;
			for (int i = 0; i < res.length; i++) {
				xAxis[i] = z;
				z++;
			}
			graph.setXAxis(xAxis);
			graph.draw(rowNum, res);
		}
		//Hours
		if (currentIndex.equals(INDICES_TYPES[3]) && hour) {
			graph.clear(rowNum);
			setTable(3);
			double[] xAxis = new double[res.length];
			int z = 0;
			for (int i = 0; i < res.length; i++) {
				xAxis[i] = z;
				z++;
			}
			graph.setXAxis(xAxis);
			graph.draw(rowNum, res);
		}
		//Minutes
		if (currentIndex.equals(INDICES_TYPES[4]) && minute) {
			graph.clear(rowNum);
			setTable(4);
			double[] xAxis = new double[res.length];
			int z = 0;
			for (int i = 0; i < res.length; i++) {
				xAxis[i] = z;
				z++;
			}
			graph.setXAxis(xAxis);
			graph.draw(rowNum, res);
		}

		// Resets view
		autosizeGraph();
	}

	/**
	 * AutoResizes graph window
	 */
	private void autosizeGraph() {
		graph.fillPlot();
	}

	/**
	 * @return the panel's name
	 */
	@Override
	public String getName() {
		return "Arrival Values";
	}

	@Override
	public void lostFocus() {
		ew.setLastPanel(TRAFFIC_GRAPHARRIVAL_PANEL);
	}

}
