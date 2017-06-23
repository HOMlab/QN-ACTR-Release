package jmt.gui.jwat.workloadAnalysis.tables;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.DefaultCellEditor;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTable;
import javax.swing.KeyStroke;
import javax.swing.event.TableModelEvent;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.framework.gui.table.editors.ButtonCellEditor;
import jmt.gui.common.CommonConstants;
import jmt.gui.common.resources.JMTImageLoader;
import jmt.gui.exact.table.ComboBoxCell;
import jmt.gui.jwat.JWATConstants;
import jmt.gui.jwat.workloadAnalysis.tables.listeners.RowDeleteListener;

public class JWatVariableInputTable extends JTable implements CommonConstants, JWATConstants {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/* Set of column dimensions */
	protected int[] columnSizes = new int[] { 38, 100, 60, 145, 30, 100, 30, 30, 18 };

	public static final Object[] VarTypes = new Object[] { "Numeric", "String", "Date" };
	protected JComboBox combobox = new JComboBox(VarTypes);
	//NEW 
	//public static final Object[] regularExpr = new Object[] {"\\d+.\\d+.\\d+.\\d","\\d\\d/\\w\\w\\w/\\d\\d\\d\\d:\\d\\d:\\d\\d:\\d\\d[^\\]]+"};	//Regular Expr.
	//protected JComboBox regCombo = new JComboBox(regularExpr);

	protected AbstractAction deleteVar = new AbstractAction("") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.SHORT_DESCRIPTION, "Delete");
			putValue(Action.SMALL_ICON, JMTImageLoader.loadImage("Delete"));
		}

		public void actionPerformed(ActionEvent arg0) {
			((JWatVariableInputTableModel) JWatVariableInputTable.this.getModel()).deleteRow(JWatVariableInputTable.this.getSelectedRow());
			JWatVariableInputTable.this.tableChanged(new TableModelEvent(JWatVariableInputTable.this.getModel()));
			fireDeleteEvent();
		}
	};

	protected JButton delVar = new JButton() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			setAction(deleteVar);
			setFocusable(true);
		}
	};

	// Sets a table model for visualization and editing of data
	public void setModel(JWatVariableInputTableModel tabMod) {
		super.setModel(tabMod);
		sizeColumnsAndRows();
		setRowHeight(ROW_HEIGHT);
		this.getColumnModel().getColumn(1).setCellEditor(new DefaultCellEditor(combobox));
		/*this.getColumnModel().getColumn(5).setCellEditor(
				new DefaultCellEditor(combobox));*/
		this.getColumnModel().getColumn(columnSizes.length - 1).setCellEditor(new ButtonCellEditor(new JButton(deleteVar)));
	}

	// set sizes for columns and rows of this table.
	protected void sizeColumnsAndRows() {
		for (int i = 0; i < columnSizes.length && i < getColumnCount(); i++) {
			this.getColumnModel().getColumn(i).setPreferredWidth(columnSizes[i]);
			if (i == 2) {
				this.getColumnModel().getColumn(i).setMaxWidth(columnSizes[i]);
			}
			if (i == 0) {
				this.getColumnModel().getColumn(i).setMaxWidth(columnSizes[i]);
			}
			if (i == columnSizes.length - 1) {
				// delete button and containing table cells as well, must be square
				this.getColumnModel().getColumn(i).setMaxWidth(columnSizes[i]);
				this.setRowHeight(columnSizes[i]);
			}
		}
	}

	@Override
	public TableCellRenderer getCellRenderer(int row, int column) {
		/*if (column == 5)
			return new ComboBoxCell(regularExpr);*/
		if (column == 1 || column == 3 || column == 4 || column == 5 || column == 6 || column == 7) {
			return getDefaultRenderer(String.class);
		}
		if (column == columnSizes.length - 1) {
			return new ButtonCellEditor(delVar);
		}
		if (column == 0) {
			return getDefaultRenderer(Boolean.class);
		}
		if (column == 2) {
			return new ComboBoxCell(VarTypes);
		}
		return null;
	}

	/* returns customized editor for table cells. */
	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		/*	if (column == 5) {
				return new DefaultCellEditor(regCombo);
			}*/
		if (column == 2) {
			return new DefaultCellEditor(combobox);
		} else if (column == columnSizes.length - 1) {
			return new ButtonCellEditor(new JButton(deleteVar));
		} else {
			return super.getCellEditor(row, column);
		}
	}

	//Update 19/10/2006
	protected MouseHandler mouseHandler;
	private Vector<RowDeleteListener> deleteLisener = new Vector<RowDeleteListener>(); //<RowDeleteListener>

	public void addDeleteRowListener(RowDeleteListener r) {
		if (!deleteLisener.contains(r)) {
			deleteLisener.add(r);
		}
	}

	private void fireDeleteEvent() {
		for (int i = 0; i < deleteLisener.size(); i++) {
			deleteLisener.get(i).rowsDeletedEvent();
		}
	}

	public AbstractAction CLEAR_ACTION = new AbstractAction("Clear") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false));
			putValue(Action.SHORT_DESCRIPTION, "Clears selected cells");
		}

		public void actionPerformed(ActionEvent e) {
			clearCells();
		}
	};

	public AbstractAction DESEL_ACTION = new AbstractAction("Deselect all") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Deselected all variable in format table");
		}

		public void actionPerformed(ActionEvent e) {
			((JWatVariableInputTableModel) dataModel).deselectAll();
			tableChanged(new TableModelEvent(dataModel));
		}
	};

	private AbstractAction deleteClass = new AbstractAction("Delete selected varaible") {
		/**
		* 
		*/
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0, false));
			putValue(Action.SHORT_DESCRIPTION, "Deletes selected variable from the format");
		}

		public void actionPerformed(ActionEvent e) {
			clearCells();
		}
	};

	private AbstractAction deselectAll = new AbstractAction("Deselect all variables") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Deselected all variable in format table");
		}

		public void actionPerformed(ActionEvent e) {
			((JWatVariableInputTableModel) dataModel).deselectAll();
			tableChanged(new TableModelEvent(dataModel));
		}
	};

	protected void installKeyboard() {
		InputMap im = getInputMap();
		ActionMap am = getActionMap();
		installKeyboardAction(im, am, CLEAR_ACTION);
		installKeyboardAction(im, am, DESEL_ACTION);
	}

	protected void installKeyboardAction(Action a) {
		installKeyboardAction(getInputMap(), getActionMap(), a);
	}

	protected void installKeyboardAction(InputMap im, ActionMap am, Action a) {
		Object name = a.getValue(Action.NAME);
		KeyStroke key = (KeyStroke) a.getValue(Action.ACCELERATOR_KEY);
		im.put(key, name);
		am.put(name, a);
	}

	private void clearCells() {
		int numrows = getSelectedRowCount();
		int[] rowsselected = getSelectedRows();
		for (int i = 0; i < numrows; i++) {
			((JWatVariableInputTableModel) dataModel).deleteRow(rowsselected[numrows - 1 - i]);
		}
		tableChanged(new TableModelEvent(dataModel));
		fireDeleteEvent();
	}

	public JWatVariableInputTable() {
		super();
		combobox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent arg0) {
				if (JWatVariableInputTable.this.getSelectedRow() != -1) {
					switch (combobox.getSelectedIndex()) {
						case NUMERIC:
							JWatVariableInputTable.this.setValueAt("", JWatVariableInputTable.this.getSelectedRow(), 4);
							JWatVariableInputTable.this.setValueAt("([+-])?\\d+([.]\\d+)?", JWatVariableInputTable.this.getSelectedRow(), 5);
							break;
						case STRING:
							JWatVariableInputTable.this.setValueAt("\"", JWatVariableInputTable.this.getSelectedRow(), 4);
							JWatVariableInputTable.this.setValueAt("\\w+", JWatVariableInputTable.this.getSelectedRow(), 5);
							break;
						case DATE:
							JWatVariableInputTable.this.setValueAt("[]", JWatVariableInputTable.this.getSelectedRow(), 4);
							JWatVariableInputTable.this.setValueAt("\\d\\d/\\w\\w\\w/\\d\\d\\d\\d:\\d\\d:\\d\\d:\\d\\d[^\\]]+",
									JWatVariableInputTable.this.getSelectedRow(), 5);
							break;
						default:
					}
				}
			}
		});
		setSelectionBackground(new Color(83, 126, 126));
		setSelectionForeground(Color.BLACK);
		installKeyboard();
		installMouse();
	}

	protected JPopupMenu makeMouseMenu() {
		JPopupMenu menu = new JPopupMenu();
		menu.add(deleteClass);
		menu.add(new JSeparator());
		menu.add(deselectAll);
		return menu;
	}

	protected void installMouse() {
		mouseHandler = new MouseHandler(makeMouseMenu());
		mouseHandler.install();
	}

	private void controlSelection() {
		if (getSelectedRow() != -1) {
			CLEAR_ACTION.setEnabled(true);
			deleteClass.setEnabled(true);
		} else {
			CLEAR_ACTION.setEnabled(false);
			deleteClass.setEnabled(false);
		}
	}

	protected class MouseHandler extends MouseAdapter {

		private boolean isInstalled;
		private JPopupMenu menu;

		public MouseHandler(JPopupMenu menu) {
			this.menu = menu;
		}

		public void install() {
			if (!isInstalled) {
				addMouseListener(this);
				isInstalled = true;
			}
		}

		public void uninstall() {
			if (isInstalled) {
				removeMouseListener(this);
				isInstalled = false;
			}
		}

		@Override
		public void mousePressed(MouseEvent e) {
			processME(e);
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			processME(e);
		}

		protected void processME(MouseEvent e) {
			if (e.isPopupTrigger()) {
				controlSelection();
				menu.show(e.getComponent(), e.getX(), e.getY());
			}
		}

		public JPopupMenu getMenu() {
			return menu;
		}

	}

}
