package jmt.gui.jwat.workloadAnalysis.tables;

import javax.swing.table.AbstractTableModel;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.gui.jwat.JWATConstants;

public class JWatUnivariateStatsTableModel extends AbstractTableModel implements JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// TODO: Deve essergli passata una istanza di matrixOsservazioni o array di variabili
	// Nomi delle colonne della tabella
	protected String[] columnNames = new String[] { "Variable", "Mean", "Variance", "Std. Dev.", "Coeff. of var.", "Minimum", "Maximum", "Range",
			"Median", "Kurtosis", "Skewness", "Num. Obs." };

	private MatrixOsservazioni data = null;

	//private DecimalFormat format = new DecimalFormat("####.####E0");

	public JWatUnivariateStatsTableModel(MatrixOsservazioni model) {
		data = model;
	}

	public int getRowCount() {
		if (data == null) {
			return 0;
		}
		return data.getVariables().length;
	}

	public int getColumnCount() {
		return columnNames.length;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		if (rowIndex >= 0) {
			if (columnIndex == 0) {
				return data.getVariables()[rowIndex].getName();
			}
			if (columnIndex == 1) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getMean());
			}
			if (columnIndex == 2) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getVariance());
			}
			if (columnIndex == 3) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getDevStd());
			}
			if (columnIndex == 4) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getDevStd()
						/ data.getVariables()[rowIndex].getUniStats().getMean());
			}
			if (columnIndex == 5) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getMinValue());
			}
			if (columnIndex == 6) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getMaxValue());
			}
			if (columnIndex == 7) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getRangeValue());
			}
			if (columnIndex == 8) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getMedian());
			}
			if (columnIndex == 9) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getKurtosis());
			}
			if (columnIndex == 10) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getSkewness());
			}
			if (columnIndex == 11) {
				return defaultFormat.format(data.getVariables()[rowIndex].getUniStats().getNumObs());
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
		if (columnIndex < columnNames.length) {
			return columnNames[columnIndex];
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

	@Override
	public Class getColumnClass(int index) {
		return String.class;
	}

	public void setMatrixObs(MatrixOsservazioni m) {
		data = m;
	}
}
