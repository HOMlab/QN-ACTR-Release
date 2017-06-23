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

package jmt.framework.gui.table.editors;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.CellEditor;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

/**
 * <p>Title: Color Cell Editor</p>
 * <p>Description: Implements both a TableCellRender component and a TableCellEditor to add
 * support for managing colors into table's field. The render will display a rectangle
 * with defined Color while the editor will show a panel to chose the color from.</p>
 * 
 * @author Bertoli Marco
 *         Date: 15-giu-2005
 *         Time: 12.27.22
 */
public class ColorCellEditor extends AbstractCellEditor implements TableCellEditor, CellEditor, ActionListener, TableCellRenderer {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected Color currentColor;
	protected JButton button;
	protected static JColorChooser colorChooser = new JColorChooser();
	protected static final String EDIT = "edit";

	protected Border unselectedBorder = null;
	protected Border selectedBorder = null;
	protected JLabel label = new JLabel();

	/**
	 * Creates a new ColorCellEditor. This object will act as a Renderer or Editor.
	 */
	public ColorCellEditor() {
		button = new JButton();
		button.setActionCommand(EDIT);
		button.addActionListener(this);
		button.setBorderPainted(false);
		label.setOpaque(true);
	}

	/**
	 * Handles events from the editor button and from
	 * the dialog's OK button.
	 */
	public void actionPerformed(ActionEvent e) {
		if (EDIT.equals(e.getActionCommand())) {
			//The user has clicked the cell, so
			//bring up the dialog.
			button.setBackground(currentColor);
			colorChooser.setColor(currentColor);
			JDialog dialog = JColorChooser.createDialog(button, "Choose User Class Color", true, //modal
					colorChooser, this, //OK button handler
					null); //no CANCEL button handler
			Dimension scrDim = Toolkit.getDefaultToolkit().getScreenSize();
			dialog.setBounds((scrDim.width - dialog.getWidth()) / 2, (scrDim.height - dialog.getHeight()) / 2, dialog.getWidth(), dialog.getHeight());
			dialog.setVisible(true);

			//Make the renderer reappear.
			fireEditingStopped();

		} else { //User pressed dialog's "OK" button.
			currentColor = colorChooser.getColor();
		}
	}

	//Implement the one CellEditor method that AbstractCellEditor doesn't.
	public Object getCellEditorValue() {
		return currentColor;
	}

	//Implement the one method defined by TableCellEditor.
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
		currentColor = (Color) value;
		return button;
	}

	/**
	 *  Returns TableCellRendererComponent used to show a rectangle of the right color in the table
	 */
	public Component getTableCellRendererComponent(JTable table, Object color, boolean isSelected, boolean hasFocus, int row, int column) {
		Color newColor = (Color) color;
		label.setBackground(newColor);
		if (isSelected) {
			if (selectedBorder == null) {
				selectedBorder = BorderFactory.createMatteBorder(2, 5, 2, 5, table.getSelectionBackground());
			}
			label.setBorder(selectedBorder);
		} else {
			if (unselectedBorder == null) {
				unselectedBorder = BorderFactory.createMatteBorder(2, 5, 2, 5, table.getBackground());
			}
			label.setBorder(unselectedBorder);
		}
		return label;
	}

}
