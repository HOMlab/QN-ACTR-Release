package jmt.gui.jwat;

import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

public class ColumnHeaderToolTips extends MouseMotionAdapter {
	// Current column whose tooltip is being displayed.
	// This variable is used to minimize the calls to setToolTipText().
	TableColumn curCol;

	// Maps TableColumn objects to tooltips
	Map<TableColumn, String> tips = new HashMap<TableColumn, String>();

	// If tooltip is null, removes any tooltip text.
	public void setToolTip(TableColumn col, String tooltip) {
		if (tooltip == null) {
			tips.remove(col);
		} else {
			tips.put(col, tooltip);
		}
	}

	@Override
	public void mouseMoved(MouseEvent evt) {
		TableColumn col = null;
		JTableHeader header = (JTableHeader) evt.getSource();
		JTable table = header.getTable();
		TableColumnModel colModel = table.getColumnModel();
		int vColIndex = colModel.getColumnIndexAtX(evt.getX());

		// Return if not clicked on any column header
		if (vColIndex >= 0) {
			col = colModel.getColumn(vColIndex);
		}

		if (col != curCol) {
			header.setToolTipText(tips.get(col));
			curCol = col;
		}
	}
}