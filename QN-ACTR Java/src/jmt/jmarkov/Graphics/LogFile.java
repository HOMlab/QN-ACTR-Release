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
 *  
 *
 */
package jmt.jmarkov.Graphics;

import java.awt.Component;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.LinkedList;

import javax.swing.JOptionPane;

import jmt.jmarkov.utils.Formatter;

// this is using for saving log in to the csv file
public class LogFile implements Notifier {
	private final static String newline = System.getProperty("line.separator");
	public final static int TABBED_DELIMITED = 0;
	public final static int COMMA_DELIMITED = 1;
	public final static int SEMI_COLON_DELIMITED = 2;
	private LinkedList<Object> tempList = new LinkedList<Object>();
	private int lastGeneratedRow = 0;

	private static String columnNames[] = { "Cust. ID", "Arrival Time", "Start Execution", "Server ID", "Exit System" };
	private int delimiterType;
	private Writer output;
	private boolean logging = false;

	/**
	 * 
	 */
	public LogFile() {
		super();
	}

	private void writeFile(int row, int column, Object value) {
		//this while is generate a new row for this job(note: the id is starting from 1)
		while (lastGeneratedRow < row) {
			lastGeneratedRow++;
			tempList.add(new Integer(lastGeneratedRow));
			tempList.add(null);
			tempList.add(null);
			tempList.add(null);
			tempList.add(null);
		}

		if (column == 0) {
			return;
		}

		int i = 0;//searching the given id element in the list
		while (((Integer) tempList.get(i * 5)).intValue() != row) {
			i++;
		}
		tempList.set(i * 5 + column, value);

		while (tempList.size() > 0 && tempList.get(4) != null) {
			try {
				for (int k = 0; k < 5; k++) {
					output.write(tempList.remove().toString());
					if (delimiterType == TABBED_DELIMITED) {
						output.write("\t");
					} else if (delimiterType == COMMA_DELIMITED) {
						output.write(",");
					} else if (delimiterType == SEMI_COLON_DELIMITED) {
						output.write(";");
					}
				}
				output.write(newline);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	private void setLogValue(int row, int column, Object value) {
		if (logging) {
			writeFile(row, column, value);
		}
	}

	private void setLogValue(int row, int column, String value) {
		setLogValue(row, column, (Object) value);
	}

	private void setLogValue(int row, int column, int value) {
		setLogValue(row, column, Formatter.formatNumber(value, 0, false));
	}

	private void setLogValue(int row, int column, double value) {
		setLogValue(row, column, Formatter.formatNumber(value, 2, false));
	}

	public void enterProcessor(int jobId, int processorId, double time, double executionTime) {
		if (logging) {
			setLogValue(jobId, 2, time / 1000);
			setLogValue(jobId, 3, processorId);
		}
	}

	public void enterQueue(int jobId, double time) {
		if (logging) {
			setLogValue(jobId, 0, jobId);
			setLogValue(jobId, 1, time / 1000);
		}
	}

	public void exitProcessor(int jobId, int processorId, double time) {
		if (logging) {
			setLogValue(jobId, 4, time / 1000);
		}
	}

	public void exitQueue(int jobId, double time) {
	}

	public void exitSystem(int jobId, int processorId, double enterQueueTime, double enterCpuTime, double exitSystemTime) {
	}

	public void jobLost(int jobId, double time) {
		if (logging) {
			setLogValue(jobId, 0, jobId);
			setLogValue(jobId, 1, time / 1000);
			setLogValue(jobId, 2, "Dropped");
			setLogValue(jobId, 3, "Dropped");
			setLogValue(jobId, 4, "Dropped");
		}
	}

	public void reset() {
		if (logging) {
			Object tempObj;
			while (tempList.size() > 0) {
				try {
					for (int k = 0; k < 5; k++) {
						tempObj = tempList.remove();
						if (tempObj != null) {
							output.write(tempObj.toString());
						}

						if (delimiterType == TABBED_DELIMITED) {
							output.write("\t");
						} else if (delimiterType == COMMA_DELIMITED) {
							output.write(",");
						} else if (delimiterType == SEMI_COLON_DELIMITED) {
							output.write(";");
						}
					}
					output.write(newline);
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			try {
				output.close();
				output = null;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		tempList.clear();
		setLogging(false);
		lastGeneratedRow = 0;

	}

	public void updateProcessor(int jobId, int processorId, double remainingTime, double time) {
	}

	public void updateQueue(int jobId, double time) {
	}

	public void setLogFile(File file, int type, Component parentC) {
		delimiterType = type;
		try {
			// use buffering
			output = new BufferedWriter(new FileWriter(file));
			for (String columnName : columnNames) {
				output.write(columnName);
				if (delimiterType == TABBED_DELIMITED) {
					output.write("\t");
				} else if (delimiterType == COMMA_DELIMITED) {
					output.write(",");
				} else if (delimiterType == SEMI_COLON_DELIMITED) {
					output.write(";");
				}
			}
			output.write(newline);

		} catch (FileNotFoundException e) {
			JOptionPane.showMessageDialog(parentC, e.getMessage(), "File not found", JOptionPane.ERROR_MESSAGE);
			System.exit(0);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void refresh() {
		if (logging) {
			try {
				output.flush();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public boolean isLogging() {
		return logging;
	}

	public void setLogging(boolean logging) {
		this.logging = logging;
	}

}
