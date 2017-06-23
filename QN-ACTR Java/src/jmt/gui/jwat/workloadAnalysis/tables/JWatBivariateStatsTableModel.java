package jmt.gui.jwat.workloadAnalysis.tables;

import javax.swing.table.AbstractTableModel;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.StatBivariate;
import jmt.gui.jwat.JWATConstants;

//TODO: al modello deve essere passati i nomi delle variabili e le statisctiche bivariate
public class JWatBivariateStatsTableModel extends AbstractTableModel implements JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String[] names = new String[0];
	private MatrixOsservazioni matrix = null;

	//private DecimalFormat format = new DecimalFormat("####.####E0");

	public JWatBivariateStatsTableModel(MatrixOsservazioni m) {
		matrix = m;
	}

	/**
	 * Returns name for each column (given its index) to be displayed inside
	 * table header
	 */
	@Override
	public String getColumnName(int columnIndex) {
		if (matrix != null) {
			names = matrix.getVariableNames();
		}
		if (columnIndex < names.length) {
			return names[columnIndex];
		} else {
			return null;
		}
	}

	public int getRowCount() {
		if (matrix == null) {
			return 0;
		}
		return names.length;
	}

	public int getColumnCount() {
		return names.length;
	}

	public Object getValueAt(int rowIndex, int columnIndex) {
		if (matrix != null) {
			StatBivariate b = matrix.getBivStatObj();

			if (rowIndex == columnIndex) {
				return null;
			}
			return defaultFormat.format(b.getCovariance(rowIndex, columnIndex));
		}
		return null;
	}

	@Override
	public Class getColumnClass(int index) {
		return String.class;
	}

	public String[] getNames() {
		if (matrix != null) {
			return matrix.getVariableNames();
		}
		return names;
	}

	public void setMatrixObs(MatrixOsservazioni m) {
		matrix = m;
		if (matrix == null) {
			names = new String[0];
		} else {
			names = matrix.getVariableNames();
		}
		this.fireTableStructureChanged();
	}
}
