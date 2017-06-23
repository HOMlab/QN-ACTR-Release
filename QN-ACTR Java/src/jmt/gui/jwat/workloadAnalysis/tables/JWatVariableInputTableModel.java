package jmt.gui.jwat.workloadAnalysis.tables;

import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.table.AbstractTableModel;

import jmt.engine.jwat.filters.FilterOnVariable;
import jmt.engine.jwat.input.Parameter;

public class JWatVariableInputTableModel extends AbstractTableModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	// Names of columns contained in table. Columns containing buttons have
	// empty names
	protected String[] columnNames = new String[] { "Select", "Name", "Type", "Comment", "Sep.", "Perl5 Reg. Exp.", "Def.", "Rep.", "" };

	// Class declarations for this table's columns.
	protected Class[] colClasses = new Class[] { Boolean.class, String.class, JComboBox.class, String.class, String.class, String.class,
			String.class, String.class, JButton.class };

	/*protected Class[] colClasses = new Class[] { String.class, JComboBox.class,
			Boolean.class, String.class, String.class, JComboBox.class,
			 String.class, String.class,JButton.class };*/

	// Vettori dei valori
	private Vector<Object> names = new Vector<Object>();

	private Vector types = new Vector();

	private Vector<Object> selected = new Vector<Object>();

	private Vector comments = new Vector();

	private Vector delimiters = new Vector();

	private Vector regExpr = new Vector();

	private Vector defaults = new Vector();

	private Vector replaces = new Vector();

	private int numVar = 0;

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

	@Override
	public Class getColumnClass(int index) {
		return colClasses[index];
	}

	/**
	 * Tells wether data contained in a specific cell(given row and column
	 * index) is editable or not. In this case distribution column is not
	 * editable, as editing functionality is implemented via edit button
	 */
	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return true;
	}

	/**
	 * Numero delle righe
	 */
	public int getRowCount() {
		return names.size();
	}

	/**
	 * Numero delle colonne
	 */
	public int getColumnCount() {
		return columnNames.length;
	}

	public Object getValueAt(int row, int col) {
		if (row < names.size()) {
			switch (col) {
				case 1:
					return names.get(row);
				case 2:
					return types.get(row);
				case 0:
					return selected.get(row);
				case 3:
					return comments.get(row);
				case 4:
					return delimiters.get(row);
				case 5:
					return regExpr.get(row);
				case 6:
					return defaults.get(row);
				case 7:
					return replaces.get(row);
				case 8:
					return null;
			}
		}
		return null;
	}

	@Override
	public void setValueAt(Object value, int r, int c) {
		if (r < names.size()) {
			switch (c) {
				case 1:
					names.set(r, value);
					break;
				case 2:
					types.set(r, value);
					break;
				case 0:
					selected.set(r, value);
					break;
				case 3:
					comments.set(r, value);
					break;
				case 4:
					delimiters.set(r, value);
					break;
				case 5:
					regExpr.set(r, value);
					break;
				case 6:
					defaults.set(r, value);
					break;
				case 7:
					replaces.set(r, value);
					break;
			}
		}
		fireTableCellUpdated(r, c);
	}

	public void addNewRow() {
		//Add Default Row
		names.add("Variable " + numVar++);
		types.add("Numeric");
		selected.add(Boolean.TRUE);
		comments.add("");
		delimiters.add("");
		regExpr.add("([+-])?\\d+([.]\\d+)?");
		defaults.add("");
		replaces.add("");
	}

	public void deleteRow(int index) {
		if (index < names.size()) {
			names.remove(index);
			types.remove(index);
			comments.remove(index);
			delimiters.remove(index);
			selected.remove(index);
			regExpr.remove(index);
			defaults.remove(index);
			replaces.remove(index);
		}
	}

	public void resetTable() {
		int num = names.size();
		for (int i = 0; i < num; i++) {
			deleteRow(0);
		}
		numVar = 0;
	}

	public void clearTable() {
		while (names.size() != 0) {
			deleteRow(0);
		}
		numVar = 0;
	}

	public Vector<Object> getVarNames() {
		return names;
	}

	public String getType(int index) {
		if (index >= 0 && index < types.size()) {
			return (String) types.get(index);
		}
		return null;
	}

	// Numero di righe nella tabella
	public int getSize() {
		return names.size();
	}

	// Controlla se per tutte le righe sono specificati i valori richiesti
	public boolean checkInfos() {
		boolean check = false;
		if (names.size() == 0) {
			return false;
		}
		for (int i = 0; i < names.size(); i++) {
			if (names.get(i).equals("")) {
				return false;
			}
			if (types.get(i).equals("")) {
				return false;
			}
			if (regExpr.get(i).equals("")) {
				return false;
			}
			// if(comments.get(i).equals("")) return false;
			// if(delimiters.get(i).equals("")) return false;
			if (((Boolean) selected.get(i)).booleanValue()) {
				check = true;
			}
		}
		return check;
	}

	public void addNewRow(String name, String type, String comment, String del, String reg, String def, String rep) {
		names.add(name);
		types.add(type);
		selected.add(Boolean.TRUE);
		comments.add(comment);
		delimiters.add(del);
		regExpr.add(reg);
		defaults.add(def);
		replaces.add(rep);
		numVar++;
	}

	public Parameter getParameter() {
		int numVars = 0; //Variabili che comporranno l'osservazione
		//Creazione array delle variabili selezione
		boolean[] boolVariables = new boolean[names.size()];
		String[] nameVariables = new String[names.size()];
		int[] typeVariables = new int[names.size()];
		String[] regExpVariable = new String[names.size()];
		String[] delim = new String[delimiters.size()];
		String[] def = new String[defaults.size()];
		String[] rep = new String[replaces.size()];
		for (int i = 0; i < selected.size(); i++) {
			if (((Boolean) selected.get(i)).booleanValue()) {
				numVars++;
			}
			boolVariables[i] = ((Boolean) selected.get(i)).booleanValue();
			nameVariables[i] = ((String) names.get(i));
			typeVariables[i] = getTypeInt(i);
			regExpVariable[i] = ((String) regExpr.get(i));
			if (!((String) delimiters.get(i)).equals("")) {
				delim[i] = ((String) delimiters.get(i));
			}
			def[i] = ((String) defaults.get(i));
			rep[i] = ((String) replaces.get(i));
		}
		FilterOnVariable filter = null;
		return new Parameter(boolVariables, typeVariables, regExpVariable, delim, nameVariables, 0, filter, null, def, rep);
	}

	private int getTypeInt(int pos) {
		for (int i = 0; i < JWatVariableInputTable.VarTypes.length; i++) {
			if (types.get(pos).equals(JWatVariableInputTable.VarTypes[i])) {
				return i;
			}
		}
		return -1;
	}

	public Vector<Object> getNames() {
		return names;
	}

	public Vector getComments() {
		return comments;
	}

	public Vector getDelimiters() {
		return delimiters;
	}

	public Vector getRegExpr() {
		return regExpr;
	}

	public Vector getDefaults() {
		return defaults;
	}

	public Vector getReplaces() {
		return replaces;
	}

	public int[] getTypes() {
		int[] t = new int[types.size()];
		for (int i = 0; i < t.length; i++) {
			t[i] = getTypeInt(i);
		}
		return t;
	}

	public void deselectAll() {
		for (int i = 0; i < names.size(); i++) {
			selected.set(i, Boolean.FALSE);
		}
	}

	/** Traffic analysis control. Check if one and only one date variable is currently selected
	 **/
	public boolean checkTrafficRequirements() {
		int count = 0;
		for (int i = 0; i < names.size(); i++) {
			if (((Boolean) selected.get(i)).booleanValue()) {
				if (!types.get(i).equals(JWatVariableInputTable.VarTypes[2]) || count > 0) {
					return false;
				}
				count++;
			}
		}
		if (count == 0) {
			return false;
		}
		return true;
	}
	
	public boolean checkFittingRequirements() {
		int count = 0;
		for (int i = 0; i < names.size(); i++) {
			if (((Boolean) selected.get(i)).booleanValue()) {
				if (!types.get(i).equals(JWatVariableInputTable.VarTypes[0]) || count > 0) {
					return false;
				}
				count++;
			}
		}
		if (count == 0) {
			return false;
		}
		return true;
	}
}
