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

package jmt.gui.exact.table;

import java.awt.Component;
import java.awt.Container;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.StringTokenizer;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableModelEvent;
import javax.swing.plaf.UIResource;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableModel;

import jmt.gui.common.resources.JMTImageLoader;

/**

 * @author alyf (Andrea Conti)
 * Date: 15-set-2003
 * Time: 20.39.53

 */

/**
 * a table with some improvements:
 * <li>has a click-aware header
 * <li>has a click-aware row header
 * <li>"more rows" and "more columns" warning icons
 * <li>clipboard transfer of cell values (support depends on the underlying data model) - CTRL+C, CTRL+X, CTRL+V
 * <li>can clear selected cells - DELETE
 * <li>can fill cells with the value from a cell (if using an ExactTableModel) - CTRL+F
 */
public class ExactTable extends JTable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private static final boolean DEBUG = false;

	protected RowHeader rowHeader;
	protected int rowHeaderWidth = 80;
	protected JButton selectAllButton;
	protected JLabel moreColumnsLabel, moreRowsLabel;
	protected boolean displaysScrollLabels = true;
	protected boolean selectAllEnabled = true;
	protected boolean clipboardTransferEnabled = false;
	protected boolean fillCellsEnabled = true;
	protected TableSelectionListener selectionListener;
	protected MouseHandler mouseHandler;
	protected boolean mouseMenuEnabled = true;

	protected Clipboard clip;
	StringSelection stsel;
	boolean canPaste = false;

	protected String selectAllTooltip;//"Click to select the whole table";
	protected String moreColumnsTooltip;//"Scroll right to see more columns...";
	protected String moreRowsTooltip;//"Scroll down to see more rows...";

	protected AbstractAction selectAction = new AbstractAction() {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			if (getRowSelectionAllowed() || getColumnSelectionAllowed()) {
				putValue(Action.SHORT_DESCRIPTION, selectAllTooltip);
				putValue(Action.NAME, "*");
			} else {
				putValue(Action.SHORT_DESCRIPTION, null);
				putValue(Action.NAME, " ");
			}
		}

		public void actionPerformed(ActionEvent e) {
			stopEditing();
			selectAll();
			requestFocus();
		}
	};

	/**
	 * updates the label state when the table is scrolled
	 */
	private ChangeListener scrollListener = new ChangeListener() {
		public void stateChanged(ChangeEvent e) {
			if (displaysScrollLabels) {
				updateScrollLabels();
			}
		}
	};

	public ExactTable(TableModel dm) {
		super(dm);

		/* apply default settings */
		setDefaultRenderer(Object.class, new ExactCellRenderer());
		setDefaultEditor(Object.class, new ExactCellEditor());

		/* try to resize the columns in a not-so-stupid way */
		sizeColumns();

		moreColumnsLabel = new JLabel();
		moreRowsLabel = new JLabel();
		selectAllButton = new JButton(selectAction);

		rowHeader = new RowHeader(dataModel);

		selectionModel.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		columnModel.getSelectionModel().setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);

		setColumnSelectionAllowed(true);
		setRowSelectionAllowed(true);

		//Dall'Orso 20/12/2004
		//BEGIN
		setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
		//END

		installKeyboard();
		installMouse();

		selectionListener = new TableSelectionListener() {
			@Override
			protected void selectionChanged(JTable table, ListSelectionEvent e, int type) {
				updateActions();
			}
		};
		selectionListener.install(this);

		clip = Toolkit.getDefaultToolkit().getSystemClipboard();

	}

	protected void installKeyboard() {
		InputMap im = getInputMap();
		ActionMap am = getActionMap();
		installKeyboardAction(im, am, COPY_ACTION);
		installKeyboardAction(im, am, CUT_ACTION);
		installKeyboardAction(im, am, PASTE_ACTION);
		installKeyboardAction(im, am, FILL_ACTION);
		installKeyboardAction(im, am, CLEAR_ACTION);
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

	protected void installMouse() {
		mouseHandler = new MouseHandler(makeMouseMenu());
		mouseHandler.install();
	}

	protected JPopupMenu makeMouseMenu() {
		JPopupMenu menu = new JPopupMenu();
		menu.add(COPY_ACTION);
		menu.add(CUT_ACTION);
		menu.add(PASTE_ACTION);
		menu.add(CLEAR_ACTION);
		menu.add(FILL_ACTION);
		return menu;
	}

	/**
	 * Overridden to make sure column sizes are mantained
	 */
	@Override
	public void tableChanged(TableModelEvent e) {
		super.tableChanged(e);
		if ((e == null || e.getFirstRow() == TableModelEvent.HEADER_ROW)) {
			sizeColumns();
		}
	}

	private void sizeColumns() {

		/* try and resize columns according to their header and (if available) prototype width*/
		/* taken from Sun's JTable tutorial, with some added features */
		int colnum = dataModel.getColumnCount();
		TableColumn col;
		Component comp;
		PrototypedTableModel ptm = null;
		boolean hasPrototypes = false;
		int autoWidth;

		if (dataModel instanceof PrototypedTableModel) {
			hasPrototypes = true;
			ptm = (PrototypedTableModel) dataModel;
		}

		if (tableHeader == null) {
			return; // hack: skip column sizing until we actually have a header
		}

		for (int i = 0; i < colnum; i++) {
			col = columnModel.getColumn(i);

			comp = tableHeader.getDefaultRenderer().getTableCellRendererComponent(null, col.getHeaderValue(), false, false, 0, i);
			autoWidth = comp.getPreferredSize().width;

			if (hasPrototypes) {
				/*                comp = getDefaultRenderer(dataModel.getColumnClass(i)).
								        getTableCellRendererComponent(
								                this, ptm.getPrototype(i),
								                false, false, 0, i);
				*/comp = getDefaultRenderer(Object.class).getTableCellRendererComponent(this, ptm.getPrototype(i), false, false, 0, i);

				autoWidth = Math.max(autoWidth, comp.getPreferredSize().width);
			}

			col.setPreferredWidth(autoWidth);
		}
	}

	@Override
	public void setColumnSelectionAllowed(boolean allowed) {
		super.setColumnSelectionAllowed(allowed);
		allowed = getColumnSelectionAllowed();
		if (tableHeader instanceof ClickableTableHeader) {
			((ClickableTableHeader) tableHeader).setAllowsClickColumnSelection(allowed);
		}
		setSelectAllStatus(allowed || getRowSelectionAllowed());
	}

	@Override
	public void setRowSelectionAllowed(boolean allowed) {
		super.setRowSelectionAllowed(allowed);
		allowed = getRowSelectionAllowed();
		if (rowHeader != null) {
			rowHeader.setAllowsClickRowSelection(allowed);
		}
		setSelectAllStatus(allowed || getColumnSelectionAllowed());
	}

	public void setSelectAllStatus(boolean allowed) {
		if (allowed && !selectAllEnabled) {
			if (selectAction != null) {
				selectAction.setEnabled(true);
				selectAction.putValue(Action.NAME, "*");
				selectAction.putValue(Action.SHORT_DESCRIPTION, selectAllTooltip);
			}
			selectAllEnabled = true;
		} else if (!allowed && selectAllEnabled) {
			if (selectAction != null) {
				selectAction.setEnabled(false);
				selectAction.putValue(Action.NAME, " ");
				selectAction.putValue(Action.SHORT_DESCRIPTION, null);
			}
			selectAllEnabled = false;
		}
	}

	/**
	 * Overridden to return our nifty header
	 */
	@Override
	protected JTableHeader createDefaultTableHeader() {
		return new ClickableTableHeader(columnModel);
	}

	/**
	 * duplicated to avoid repeating the same checks multiple times
	 */
	@Override
	protected void configureEnclosingScrollPane() {
		Container p = getParent();
		if (p instanceof JViewport) {
			Container gp = p.getParent();
			if (gp instanceof JScrollPane) {
				JScrollPane scrollPane = (JScrollPane) gp;
				// Make certain we are the viewPort's view and not, for
				// example, the rowHeaderView of the scrollPane -
				// an implementor of fixed columns might do this.
				JViewport viewport = scrollPane.getViewport();
				if (viewport == null || viewport.getView() != this) {
					return;
				}
				scrollPane.setColumnHeaderView(getTableHeader());
				Border border = scrollPane.getBorder();
				if (border == null || border instanceof UIResource) {
					scrollPane.setBorder(UIManager.getBorder("Table.scrollPaneBorder"));
				}
				//rowHeader=new RowHeader(dataModel);
				rowHeader.install(this, scrollPane);
				installLabels(scrollPane);
				installSelectAllButton(scrollPane);
				viewport.addChangeListener(scrollListener);
			}
		}
	}

	@Override
	protected void unconfigureEnclosingScrollPane() {
		Container p = getParent();
		if (p instanceof JViewport) {
			Container gp = p.getParent();
			if (gp instanceof JScrollPane) {
				JScrollPane scrollPane = (JScrollPane) gp;
				// Make certain we are the viewPort's view and not, for
				// example, the rowHeaderView of the scrollPane -
				// an implementor of fixed columns might do this.
				JViewport viewport = scrollPane.getViewport();
				if (viewport == null || viewport.getView() != this) {
					return;
				}
				scrollPane.setColumnHeaderView(null);
				scrollPane.setRowHeaderView(null);

				viewport.removeChangeListener(scrollListener);
				rowHeader.uninstall();
			}
		}

	}

	private void installSelectAllButton(JScrollPane scrollPane) {
		selectAllButton.setFocusable(false);
		selectAllButton.setBorder(UIManager.getBorder("TableHeader.cellBorder"));
		scrollPane.setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, selectAllButton);
	}

	private void installLabels(JScrollPane scrollPane) {
		moreColumnsLabel.setIcon(JMTImageLoader.loadImage("table_rightarrow"));
		moreColumnsLabel.setHorizontalAlignment(SwingConstants.CENTER);
		moreColumnsLabel.setToolTipText(moreColumnsTooltip);
		moreColumnsLabel.setVisible(false);

		moreRowsLabel.setIcon(JMTImageLoader.loadImage("table_downarrow"));
		moreRowsLabel.setHorizontalAlignment(SwingConstants.CENTER);
		moreRowsLabel.setToolTipText(moreRowsTooltip);
		moreRowsLabel.setVisible(false);

		scrollPane.setCorner(ScrollPaneConstants.UPPER_RIGHT_CORNER, moreColumnsLabel);
		scrollPane.setCorner(ScrollPaneConstants.LOWER_LEFT_CORNER, moreRowsLabel);

		if (displaysScrollLabels) {
			updateScrollLabels();
		}

	}

	/**
	 *  Make sure we're not editing a cell. NOTE: requires the editor to honor stopCellEditing() or
	 *  cancelCellEditing() calls.
	 */
	public void stopEditing() {
		if (cellEditor != null) {
			if (!cellEditor.stopCellEditing()) {
				cellEditor.cancelCellEditing();
			}
		}
	}

	/**
	 * selects a cell
	 */
	public void setSelectedCell(int row, int col) {
		setColumnSelectionInterval(col, col);
		setRowSelectionInterval(row, row);
	}

	/**
	 * Overridden to stop editing
	 */
	@Override
	public void selectAll() {
		stopEditing();
		super.selectAll();
	}

	/**
	 * Updates the state of the actions. called whenever the selection state of the table changes.
	 */
	protected void updateActions() {
		int cols = getSelectedColumnCount();
		int rows = getSelectedRowCount();
		boolean somethingSelected = (rows > 0 || cols > 0);
		boolean moreSelected = (rows > 1 || cols > 1);
		COPY_ACTION.setEnabled(clipboardTransferEnabled && somethingSelected);
		CUT_ACTION.setEnabled(clipboardTransferEnabled && somethingSelected);
		CLEAR_ACTION.setEnabled(somethingSelected);
		PASTE_ACTION.setEnabled(somethingSelected && clipboardTransferEnabled && canPaste());
		FILL_ACTION.setEnabled(fillCellsEnabled && moreSelected);
	}

	private void updateScrollLabels() {
		Rectangle vr = getVisibleRect();
		moreColumnsLabel.setVisible(vr.x + vr.width < getWidth());
		moreRowsLabel.setVisible(vr.y + vr.height < getHeight());
	}

	public void updateStructure() {
		tableChanged(new TableModelEvent(dataModel, TableModelEvent.HEADER_ROW));
		if (rowHeader != null) {
			rowHeader.update();
		}
	}

	public void update() {
		tableChanged(new TableModelEvent(dataModel));
		if (rowHeader != null) {
			rowHeader.update();
		}
	}

	public void updateRow(int row) {
		updateRows(row, row);
	}

	public void updateRows(int firstRow, int lastRow) {
		tableChanged(new TableModelEvent(dataModel, firstRow, lastRow));
		if (rowHeader != null) {
			rowHeader.updateRows(firstRow, lastRow);
		}
	}

	public int getRowHeaderWidth() {
		return rowHeaderWidth;
	}

	public void setRowHeaderWidth(int rowHeaderWidth) {
		this.rowHeaderWidth = rowHeaderWidth;
		if (rowHeader != null) {
			rowHeader.setWidth(rowHeaderWidth);
		}
	}

	public String getSelectAllTooltip() {
		return selectAllTooltip;
	}

	public void setSelectAllTooltip(String selectAllTooltip) {
		this.selectAllTooltip = selectAllTooltip;
		selectAction.putValue(Action.SHORT_DESCRIPTION, selectAllTooltip);
	}

	public String getMoreRowsTooltip() {
		return moreRowsTooltip;
	}

	public void setMoreRowsTooltip(String moreRowsTooltip) {
		this.moreRowsTooltip = moreRowsTooltip;
		if (moreRowsLabel != null) {
			moreRowsLabel.setToolTipText(moreRowsTooltip);
		}
	}

	public String getMoreColumnsTooltip() {
		return moreColumnsTooltip;
	}

	public void setMoreColumnsTooltip(String moreColumnsTooltip) {
		this.moreColumnsTooltip = moreColumnsTooltip;
		if (moreColumnsLabel != null) {
			moreColumnsLabel.setToolTipText(moreColumnsTooltip);
		}
	}

	public boolean displaysScrollLabels() {
		return displaysScrollLabels;
	}

	public void setDisplaysScrollLabels(boolean displaysScrollLabels) {
		this.displaysScrollLabels = displaysScrollLabels;
		if (!displaysScrollLabels) {
			moreColumnsLabel.setVisible(false);
			moreRowsLabel.setVisible(false);
		} else {
			updateScrollLabels();
		}
	}

	/**
	 * Try to keep the viewport aligned on column boundaries in the direction of interest
	 */
	@Override
	public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {

		if (orientation == SwingConstants.HORIZONTAL) {
			return 80;
		}
		return getRowHeight();
	}

	@Override
	public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
		/* borrowed from JTable */
		if (orientation == SwingConstants.HORIZONTAL) {
			return 5 * getScrollableUnitIncrement(visibleRect, orientation, direction);
		}
		return super.getScrollableBlockIncrement(visibleRect, orientation, direction);
	}

	public boolean isClipboardTransferEnabled() {
		return clipboardTransferEnabled;
	}

	public void setClipboardTransferEnabled(boolean clipboardTransferEnabled) {
		this.clipboardTransferEnabled = clipboardTransferEnabled;
	}

	public boolean isFillCellsEnabled() {
		return fillCellsEnabled;
	}

	public void setFillCellsEnabled(boolean fillCellsEnabled) {
		this.fillCellsEnabled = fillCellsEnabled;
	}

	public boolean isMouseMenuEnabled() {
		return mouseMenuEnabled;
	}

	public void setMouseMenuEnabled(boolean mouseMenuEnabled) {
		this.mouseMenuEnabled = mouseMenuEnabled;
	}

	/* copy/cut/paste/delete stuff -------------------------------------*/

	public void copyAnchorToSelection() {
		if ((getSelectedRowCount() == 0) && (getSelectedColumnCount() == 0)) {
			return;
		}
		if (dataModel instanceof ExactTableModel) {
			stopEditing();
			int rowFrom = selectionModel.getMinSelectionIndex();
			int rowTo = selectionModel.getMaxSelectionIndex();
			ListSelectionModel csm = columnModel.getSelectionModel();
			int colFrom = csm.getMinSelectionIndex();
			int colTo = csm.getMaxSelectionIndex();

			int anchorRow = selectionModel.getAnchorSelectionIndex();
			int anchorCol = csm.getAnchorSelectionIndex();

			((ExactTableModel) dataModel).copyCellToArea(anchorRow, anchorCol, rowFrom, rowTo, colFrom, colTo);
			updateRows(rowFrom, rowTo);
			requestFocus();
		}
	}

	public void copyCells() {
		StringBuffer sbf = new StringBuffer();
		// Check to ensure we have selected only a contiguous block of
		// cells
		int numcols = getSelectedColumnCount();
		int numrows = getSelectedRowCount();
		int[] rowsselected = getSelectedRows();
		int[] colsselected = getSelectedColumns();
		if (!((numrows - 1 == rowsselected[rowsselected.length - 1] - rowsselected[0] && numrows == rowsselected.length) && (numcols - 1 == colsselected[colsselected.length - 1]
				- colsselected[0] && numcols == colsselected.length))) {
			JOptionPane.showMessageDialog(null, "Invalid Copy Selection", "Invalid Copy Selection", JOptionPane.ERROR_MESSAGE);
			return;
		}
		for (int i = 0; i < numrows; i++) {
			for (int j = 0; j < numcols; j++) {
				sbf.append(getValueAt(rowsselected[i], colsselected[j]));
				if (j < numcols - 1) {
					sbf.append("\t");
				}
			}
			sbf.append("\n");
		}
		stsel = new StringSelection(sbf.toString()) {
			@Override
			public void lostOwnership(Clipboard clipboard, Transferable contents) {
				canPaste = false;
			}
		};
		clip.setContents(stsel, stsel);
		canPaste = true;
	}

	public void clearCells() {
		if (DEBUG) {
			System.out.println("clearCells");
		}
		boolean hasClear = false;
		ExactTableModel etm = null;
		if (dataModel instanceof ExactTableModel) {
			hasClear = true;
			etm = (ExactTableModel) dataModel;
		}

		int numcols = getSelectedColumnCount();
		int numrows = getSelectedRowCount();
		int[] rowsselected = getSelectedRows();
		int[] colsselected = getSelectedColumns();
		if (!((numrows - 1 == rowsselected[rowsselected.length - 1] - rowsselected[0] && numrows == rowsselected.length) && (numcols - 1 == colsselected[colsselected.length - 1]
				- colsselected[0] && numcols == colsselected.length))) {
			JOptionPane.showMessageDialog(null, "Invalid Clear Selection", "Invalid Clear Selection", JOptionPane.ERROR_MESSAGE);
			return;
		}
		for (int i = 0; i < numrows; i++) {
			for (int j = 0; j < numcols; j++) {
				if (hasClear) {
					etm.clear(rowsselected[i], colsselected[j]);
				} else {
					setValueAt(null, rowsselected[i], colsselected[j]);
				}
			}
		}
		updateRows(rowsselected[0], rowsselected[rowsselected.length - 1]);

	}

	public void pasteCells() {
		pasteCellsAt(selectionModel.getAnchorSelectionIndex(), columnModel.getSelectionModel().getAnchorSelectionIndex());
	}

	public void pasteCellsAt(int startRow, int startCol) {
		if (!canPaste()) {
			return;
		}
		String rowstring, value;

		int i = 0, j = 0;
		if (DEBUG) {
			System.out.println("Trying to Paste");
		}
		try {
			String trstring = (String) (clip.getContents(this).getTransferData(DataFlavor.stringFlavor));
			if (DEBUG) {
				System.out.println("String is:" + trstring);
			}
			StringTokenizer st1 = new StringTokenizer(trstring, "\n");
			for (i = 0; st1.hasMoreTokens(); i++) {
				rowstring = st1.nextToken();
				StringTokenizer st2 = new StringTokenizer(rowstring, "\t");
				for (j = 0; st2.hasMoreTokens(); j++) {
					value = st2.nextToken();
					if (startRow + i < getRowCount() && startCol + j < getColumnCount()) {
						setValueAt(value, startRow + i, startCol + j);
					}
					if (DEBUG) {
						System.out.println("Putting " + value + "at row=" + startRow + i + "column=" + startCol + j);
					}

				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		update();
		setRowSelectionInterval(startRow, Math.min(getRowCount(), startRow + i - 1));
		setColumnSelectionInterval(startCol, Math.min(getColumnCount(), startCol + j - 1));
	}

	/**
	 * As of now does not allow importing data that has not been copied by us. Ideas are welcome...
	 */
	public boolean canPaste() {
		return canPaste;
	}

	public AbstractAction COPY_ACTION = new AbstractAction("Copy") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Copies selected cells to the system clipboard");
		}

		public void actionPerformed(ActionEvent e) {
			copyCells();
		}
	};

	public AbstractAction CUT_ACTION = new AbstractAction("Cut") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Copies selected cells to the system clipboard and clears them");
		}

		public void actionPerformed(ActionEvent e) {
			copyCells();
			clearCells();
		}
	};

	public AbstractAction PASTE_ACTION = new AbstractAction("Paste") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Pastes cells from the system clipboard, starting from the currently focused cell");
		}

		public void actionPerformed(ActionEvent e) {
			pasteCells();
		}
	};

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

	public AbstractAction FILL_ACTION = new AbstractAction("Fill") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Fills selected cells with the value of the focused cell");
		}

		public void actionPerformed(ActionEvent e) {
			copyAnchorToSelection();
		}
	};

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
			if (!mouseMenuEnabled) {
				return;
			}
			if (e.isPopupTrigger()) {
				updateActions();
				menu.show(e.getComponent(), e.getX(), e.getY());
			}
		}

		public JPopupMenu getMenu() {
			return menu;
		}

	}

}
