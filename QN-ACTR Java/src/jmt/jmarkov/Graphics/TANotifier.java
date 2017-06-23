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

/*
 * Created on 16-mar-2004 by Ernesto
 *
 */
package jmt.jmarkov.Graphics;

import java.awt.Color;
import java.awt.Component;

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;

import jmt.jmarkov.Graphics.constants.DrawConstrains;
import jmt.jmarkov.Graphics.constants.DrawNormal;
import jmt.jmarkov.utils.Formatter;

// this is using for showing log
public class TANotifier extends JTable implements Notifier {
	/**
		 * 
		 */
	private static final long serialVersionUID = 1L;

	//	private final static String newline = System.getProperty("line.separator");
	DefaultTableModel model;
	//	private int MAXIMUM_LOG = 1000000;

	// for using avg jobs in Q + Server
	private double totNumberOfJobsInSystem, lastUpdateTimeNumJobs;
	private int numberOfJobsInTheSystem;

	//avg utilization
	//avg service time
	private double totalServiceTime;

	//avg throughput
	private int totalCustomerExit;

	//avg response time
	private double totalResponseTime;

	//total customer arrived
	//arrival rate
	private int totalCustomerArrived;

	private int numberOfJobLost;

	private static String columnNames[] = { "", "Simulation Result", "Analytical Result" };

	private static String rowNames[] = { "Avg. Cust. N in Station (Queue+Service)",//0
			"Avg. Utilization(All Servers) U",//1
			"Avg. Throughput X",//2
			"Avg. Response Time R",//3
			"Arrival Rate (lambda)",//4
			"Avg. Service Time S",//5
			"Cust. Dropped Rate", //6
			"Total Customer Arrived",//7
			"Current Simulation Time"//8
	};

	private static String rowUnit[] = { " cust.",//0
			" ",//1
			" cust./s",//2
			" s",//3
			" cust./s",//4
			" s",//5
			" ", //6
			" cust.",//7
			" s"//8
	};

	/**
	 * 
	 */
	public TANotifier() {
		super();
		initTable();
		reset();
	}

	void initTable() {
		this.setFont(new DrawNormal().getFont());
		model = new DefaultTableModel() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public boolean isCellEditable(int row, int col) {
				return false;
			}
		};
		super.setModel(model);

		TableCellRenderer renderer = new DefaultTableCellRenderer() {
			/**
			 * 
			 */
			private static final long serialVersionUID = 1L;

			@Override
			public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
				Component cell = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
				if (!isSelected) {
					if (column == 0) {
						cell.setBackground(Color.lightGray);
					} else {
						cell.setBackground(Color.white);
					}
				} else if (column == 0) {
					cell.setBackground(Color.darkGray);
				} else {
					cell.setBackground(Color.blue);
				}

				return cell;
			}
		};
		this.setDefaultRenderer(Object.class, renderer);
		this.getTableHeader().setReorderingAllowed(false);
		for (String columnName : columnNames) {
			model.addColumn(columnName);
		}
		for (int row = 0; row < rowNames.length; row++) {
			setCell(row, 0, rowNames[row]);
		}

	}

	private void setCell(int row, int column, Object value) {
		while (model.getRowCount() - 1 < row) {
			model.addRow(new Object[] {});
		}
		if (value == null || column == 0) {
			model.setValueAt(value, row, column);
		} else {
			model.setValueAt(value.toString() + rowUnit[row], row, column);
		}
	}

	private void setCell(int row, int column, String value) {
		setCell(row, column, (Object) value);
	}

	private void setCell(int row, int column, int value) {
		setCell(row, column, Formatter.formatNumber(value, 0));
	}

	private void setCell(int row, int column, double value) {
		setCell(row, column, Formatter.formatNumber(value, 2));
	}

	private void currentTime(double time) {
		setCell(8, 1, (int) time);
	}

	private void updateJobsInSystem(double now) {
		totNumberOfJobsInSystem += (now - lastUpdateTimeNumJobs) * numberOfJobsInTheSystem;
		lastUpdateTimeNumJobs = now;
		//average jobs in Q+S : totNumberOfJobsInSystem / now; #jobs
		setCell(0, 1, totNumberOfJobsInSystem / now);
	}

	private void updateUtilization(double enterCpuTime, double exitSystemTime) {
		double now = exitSystemTime;
		totalServiceTime += exitSystemTime - enterCpuTime;
		//average utilization : totalServiceTime / now
		//average service time : totalServiceTime / totalCustomerExit 
		setCell(1, 1, totalServiceTime / now);
		setCell(5, 1, totalServiceTime / totalCustomerExit);
	}

	private void updateThroughput(double now) {
		totalCustomerExit++;
		//average throughput : totalCustomerExit / now  # jobs/s
		setCell(2, 1, totalCustomerExit / now);
	}

	private void updateResponseTime(double enterQueueTime, double exitSystemTime) {
		totalResponseTime += exitSystemTime - enterQueueTime;
		// average response time = totalResponseTime / totalcustomerexit
		setCell(3, 1, totalResponseTime / totalCustomerExit);
	}

	private void updateCustomerArrived(double now) {
		totalCustomerArrived++;
		//total customer arrived : totalCustomerArrived
		// arrival rate : totalcustomerarrived / now
		setCell(4, 1, totalCustomerArrived / now);
		setCell(7, 1, totalCustomerArrived);
	}

	private void updateJobLost() {
		numberOfJobLost++;
		//avg number of dropped job :  numberOfJobLost/ totalCustomerArrived
		setCell(6, 1, (double) numberOfJobLost / (double) totalCustomerArrived);
	}

	/**
	 * @param dCst
	 */
	public void changeDrawSettings(DrawConstrains dCst) {
		this.setFont(dCst.getFont());
	}

	public void enterProcessor(int jobId, int processorId, double time, double executionTime) {
		//		setCell(jobId, 2, time / 1000);
		//		setCell(jobId, 3, processorId);
	}

	public void enterQueue(int jobId, double time) {
		//		setCell(jobId, 0, jobId);
		//		setCell(jobId, 1, time / 1000);
		updateJobsInSystem(time / 1000);
		numberOfJobsInTheSystem++;
		updateCustomerArrived(time / 1000);
		currentTime(time / 1000);
	}

	public void exitProcessor(int jobId, int processorId, double time) {
		//		setCell(jobId, 4, time / 1000);
		updateJobsInSystem(time / 1000);
		numberOfJobsInTheSystem--;
		updateThroughput(time / 1000);
		currentTime(time / 1000);
	}

	public void exitQueue(int jobId, double time) {
		currentTime(time / 1000);
	}

	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime) {
		updateUtilization(enterCpuTime / 1000, exitSystemTime / 1000);
		updateResponseTime(enterQueueTime / 1000, exitSystemTime / 1000);
	}

	public void jobLost(int jobId, double time) {
		//		setCell(jobId, 0, jobId);
		//		setCell(jobId, 1, time / 1000);
		//		setCell(jobId, 2, "Dropped");
		//		setCell(jobId, 3, "Dropped");
		//		setCell(jobId, 4, "Dropped");
		updateCustomerArrived(time / 1000);
		updateJobLost();
		currentTime(time / 1000);
	}

	public void reset() {
		// setText("");
		//		model.setRowCount(0);
		// model.getDataVector().removeAllElements();
		totNumberOfJobsInSystem = 0;
		lastUpdateTimeNumJobs = 0;
		numberOfJobsInTheSystem = 0;

		totalServiceTime = 0;

		totalCustomerExit = 0;

		totalResponseTime = 0;

		totalCustomerArrived = 0;

		numberOfJobLost = 0;

		for (int i = 0; i < rowNames.length; i++) {
			setCell(i, 1, null);
			setCell(i, 2, null);
		}
	}

	public void updateProcessor(int jobId, int processorId, double remainingTime, double time) {
		currentTime(time / 1000);
	}

	public void updateQueue(int jobId, double time) {
		currentTime(time / 1000);

	}

	public void refresh() {

	}

	public void setAnalyticalResult(double avgCustInStation,//1
			double avgUtilization,//2
			double avgThroughput,//3
			double avgResponseTime,//4
			double arrivalRate,//5
			double serviceTime,//6
			double custDroppedRate)//7
	{
		setCell(0, 2, avgCustInStation);
		setCell(1, 2, avgUtilization);
		setCell(2, 2, avgThroughput);
		setCell(3, 2, avgResponseTime);
		setCell(4, 2, arrivalRate);
		setCell(5, 2, serviceTime);
		if (custDroppedRate == 0) {
			setCell(6, 2, "---");
		} else {
			setCell(6, 2, custDroppedRate);
		}
		setCell(7, 2, "---");
		setCell(8, 2, "---");

	}

	public void setAnalyticalResult() {
		for (int i = 0; i < rowNames.length - 2; i++) {
			setCell(i, 2, "---");
		}
	}

}
