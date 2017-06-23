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
package jmt.gui.common.editors;

import java.awt.Component;
import java.util.HashMap;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import jmt.gui.common.CommonConstants;
import jmt.gui.common.definitions.BlockingRegionDefinition;
import jmt.gui.common.definitions.ClassDefinition;
import jmt.gui.common.definitions.StationDefinition;
import jmt.gui.common.resources.JMTImageLoader;

/**
 * <p>Title: Imaged ComboBox Table CellViewer/CellEditor Factory</p>
 * <p>Description: A component that creates Comboboxes with station or class names
 * and images to be used as both a viewer and an editor in a table.
 * Uses internal caching to speed up visualization.</p>
 *
 * @author Bertoli Marco
 *         Date: 11-may-2006
 *         Time: 17.03.54
 */
public class ImagedComboBoxCellEditorFactory {
	/** Station Definition data structure */
	protected StationDefinition sd;
	/** Class Definition data structure */
	protected ClassDefinition cd;
	/** Tells if modify classes or stations */
	protected boolean isStation;
	/** Tells if modify strings with images */
	protected boolean isString;
	/** Cache for components */
	protected HashMap<Object, LabelRenderer> cache = new HashMap<Object, LabelRenderer>();
	/** Renderer instance */
	protected TableCellRenderer renderer;
	/** Editor instance */
	protected ImagedComboEditor editor;
	/** Null element renderer */
	protected LabelRenderer nullRenderer;
	/** Allows null as a valid component */
	protected boolean allowsNull = false;

	/**
	 * Creates a new ImagedComboBoxCellEditorFactory used to display
	 * station comboboxes
	 * @param sd reference to station definition datastructure
	 */
	public ImagedComboBoxCellEditorFactory(StationDefinition sd) {
		setData(sd);
	}

	/**
	 * Creates a new ImagedComboBoxCellEditorFactory used to display
	 * station comboboxes
	 * @param cd reference to class definition datastructure
	 */
	public ImagedComboBoxCellEditorFactory(ClassDefinition cd) {
		setData(cd);
	}

	/**
	 * Creates a new ImagedComboBoxCellEditorFactory used to display
	 * strings with images. Every image must be called <code>[String] + "Combo"</code>
	 * where <code>[String]</code> is the content of the string.
	 */
	public ImagedComboBoxCellEditorFactory() {
		isString = true;
		isStation = false;
	}

	/**
	 * Creates a new ImagedComboBoxCellEditorFactory used to display
	 * station types with images. Every image must be called <code>[String] + "Combo"</code>
	 * where <code>[String]</code> is the content of the string.
	 * @param isStation tells to resolve station names
	 */
	public ImagedComboBoxCellEditorFactory(boolean isStation) {
		isString = true;
		this.isStation = isStation;
	}

	/**
	 * Changes stored reference to station data structure
	 * @param sd reference to station definition datastructure
	 */
	public void setData(StationDefinition sd) {
		isStation = true;
		isString = false;
		this.sd = sd;
		clearCache();
	}

	/**
	 * Changes stored reference to station data structure
	 * @param cd reference to class definition datastructure
	 */
	public void setData(ClassDefinition cd) {
		isStation = false;
		isString = false;
		this.cd = cd;
		clearCache();
	}

	/**
	 * Clears inner component cache. Must be called each time name or
	 * type of a class/station changes.
	 */
	public void clearCache() {
		cache.clear();
		nullRenderer = null;
	}

	/**
	 * Returns an instance of editor, given search key for elements to be shown
	 * @param data array with search's key for elements to be shown
	 */
	public TableCellEditor getEditor(Object[] data) {
		if (editor == null) {
			editor = new ImagedComboEditor();
		}
		LabelRenderer[] rend;
		if (allowsNull) {
			rend = new LabelRenderer[data.length + 1];
			rend[0] = getDrawComponent(null);
			for (int i = 1; i < rend.length; i++) {
				rend[i] = getDrawComponent(data[i - 1]);
			}
		} else {
			rend = new LabelRenderer[data.length];
			for (int i = 0; i < data.length; i++) {
				rend[i] = getDrawComponent(data[i]);
			}
		}
		editor.setData(rend);
		return editor;
	}

	/**
	 * Returns an instance of editor, given search key for elements to be shown
	 * @param data vector with search's key for elements to be shown
	 */
	public TableCellEditor getEditor(List data) {
		if (editor == null) {
			editor = new ImagedComboEditor();
		}
		LabelRenderer[] rend;
		if (allowsNull) {
			rend = new LabelRenderer[data.size() + 1];
			rend[0] = getDrawComponent(null);
			for (int i = 1; i < rend.length; i++) {
				rend[i] = getDrawComponent(data.get(i - 1));
			}
		} else {
			rend = new LabelRenderer[data.size()];
			for (int i = 0; i < data.size(); i++) {
				rend[i] = getDrawComponent(data.get(i));
			}
		}
		editor.setData(rend);
		return editor;
	}

	/**
	 * Tells if comboBoxes should allow null values selection
	 * @param value true if selection is allowed, false otherwise
	 */
	public void setAllowsNull(boolean value) {
		allowsNull = value;
	}

	/**
	 * Returns component to draw a station or a class in a comboBox.
	 * Provides caching functionalities.
	 * @param key search's key for component to be shown
	 * @return created component
	 */
	protected LabelRenderer getDrawComponent(Object key) {
		LabelRenderer label;
		if (key == null) {
			if (nullRenderer == null) {
				nullRenderer = new LabelRenderer(null);
			}
			return nullRenderer;
		}

		if (cache.containsKey(key)) {
			label = cache.get(key);
		} else {
			label = new LabelRenderer(key);
			cache.put(key, label);
		}
		return label;
	}

	/**
	 * Class used to display a component in a combobox
	 */
	protected class LabelRenderer extends JLabel {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected Object key;

		/**
		 * Construst a new labelrenderer with given object
		 * @param key search's key for class or station
		 */
		public LabelRenderer(Object key) {
			super();
			setHorizontalTextPosition(SwingConstants.RIGHT);
			setOpaque(true);
			setBorder(BorderFactory.createEmptyBorder(0, 2, 0, 2));
			this.key = key;
			if (isString) {
				// This is only a string
				if (isStation) {
					// If this is a station type string, resolves label names
					setText(CommonConstants.STATION_NAMES.get(key));
				} else {
					setText((String) key);
				}
				setIcon(JMTImageLoader.loadImage(key + "Combo"));
			} else if (isStation) {
				// This is a station
				if (sd.getStationName(key) != null && !sd.getStationName(key).equals("")) {
					setText(sd.getStationName(key));
					setIcon(JMTImageLoader.loadImage(sd.getStationType(key) + "Combo"));
				}
				// This is a region
				if (sd instanceof BlockingRegionDefinition) {
					BlockingRegionDefinition bd = (BlockingRegionDefinition) sd;
					if (bd.getRegionName(key) != null && !bd.getRegionName(key).equals("")) {
						setText(bd.getRegionName(key));
						setIcon(JMTImageLoader.loadImage("BlockingCombo"));
					}
				}
				if (key == null) {
					// Null component
					setText("");
				}
			} else {
				// This is a class
				if (cd.getClassName(key) != null && !cd.getClassName(key).equals("")) {
					setText(cd.getClassName(key));
					switch (cd.getClassType(key)) {
						case CommonConstants.CLASS_TYPE_CLOSED:
							setIcon(JMTImageLoader.loadImage("ClosedCombo"));
							break;
						case CommonConstants.CLASS_TYPE_OPEN:
							setIcon(JMTImageLoader.loadImage("OpenCombo"));
							break;
					}
				} else if (key == null) {
					// Null component
					setText(CommonConstants.ALL_CLASSES);
				}

			}
		}

		/**
		 * Gets search's key for rendered object (class or station)
		 * @return search's key for rendered object (class or station)
		 */
		public Object getKey() {
			return key;
		}
	}

	/**
	 * Returns an instance of Imaged combobox renderer
	 * @return an instance of Imaged combobox renderer
	 */
	public TableCellRenderer getRenderer() {
		if (renderer == null) {
			renderer = new ImagedComboRenderer();
		}
		return renderer;
	}

	/**
	 * This class is used to display a custom renderer into the comboBox with
	 * Jlabels generated by getDrawComponent method.
	 */
	protected class ComboImageRenderer implements ListCellRenderer {

		/**
		 * Simply uses value param as a renderer (as we pass a JLabel)
		 */
		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
			JLabel component = (JLabel) value;
			if (component == null) {
				component = nullRenderer;
			}
			if (isSelected) {
				component.setBackground(list.getSelectionBackground());
				component.setForeground(list.getSelectionForeground());
			} else {
				component.setBackground(list.getBackground());
				component.setForeground(list.getForeground());
			}
			return component;
		}
	}

	protected class ImagedComboRenderer implements TableCellRenderer {
		protected JComboBox combo = new JComboBox();

		/**
		 * Creates a new ImagedComboRenderer and sets renderer for comboBox.
		 */
		public ImagedComboRenderer() {
			combo.setRenderer(new ComboImageRenderer());
		}

		/**
		 * Returns the component used for drawing the cell.  This method is
		 * used to configure the renderer appropriately before drawing.
		 *
		 * @param    table        the <code>JTable</code> that is asking the
		 * renderer to draw; can be <code>null</code>
		 * @param    value        the value of the cell to be rendered.  It is
		 * up to the specific renderer to interpret
		 * and draw the value.  For example, if
		 * <code>value</code>
		 * is the string "true", it could be rendered as a
		 * string or it could be rendered as a check
		 * box that is checked.  <code>null</code> is a
		 * valid value
		 * @param    isSelected    true if the cell is to be rendered with the
		 * selection highlighted; otherwise false
		 * @param    hasFocus    if true, render cell appropriately.  For
		 * example, put a special border on the cell, if
		 * the cell can be edited, render in the color used
		 * to indicate editing
		 * @param    row     the row index of the cell being drawn.  When
		 * drawing the header, the value of
		 * <code>row</code> is -1
		 * @param    column     the column index of the cell being drawn
		 */
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
			Component renderer = getDrawComponent(value);
			if (table.isCellEditable(row, column)) {
				// If the cell is editable, returns a comboBox
				combo.removeAllItems();
				if (value != null) {
					combo.addItem(renderer);
					combo.setSelectedItem(value);
				}
				if (!isSelected) {
					combo.setBackground(table.getBackground());
					combo.setForeground(table.getForeground());
				} else {
					combo.setBackground(table.getSelectionBackground());
					combo.setForeground(table.getSelectionForeground());
				}
				return combo;
			} else {
				// Otherwise returns the label only.
				if (!isSelected) {
					renderer.setBackground(table.getBackground());
					renderer.setForeground(table.getForeground());
				} else {
					renderer.setBackground(table.getSelectionBackground());
					renderer.setForeground(table.getSelectionForeground());
				}
				return renderer;
			}
		}
	}

	/**
	 * This is a combobox editor. It will recycle the same combobox changing items
	 */
	protected class ImagedComboEditor extends DefaultCellEditor {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		protected JComboBox combo;

		/**
		 * Creates a new ImagedComboEditor and sets renderer for comboBox.
		 */
		public ImagedComboEditor() {
			super(new JComboBox());
			combo = (JComboBox) super.getComponent();
			combo.setRenderer(new ComboImageRenderer());
		}

		/**
		 * Changes data shown in this combobox
		 * @param data array with LabelRenderers to be shown
		 */
		public void setData(LabelRenderer[] data) {
			combo.removeAllItems();
			for (LabelRenderer element : data) {
				combo.addItem(element);
			}
		}

		/**
		 * Sets an initial <code>value</code> for the editor.  This will cause
		 * the editor to <code>stopEditing</code> and lose any partially
		 * edited value if the editor is editing when this method is called. <p>
		 * <p/>
		 * Returns the component that should be added to the client's
		 * <code>Component</code> hierarchy.  Once installed in the client's
		 * hierarchy this component will then be able to draw and receive
		 * user input.
		 *
		 * @param    table        the <code>JTable</code> that is asking the
		 * editor to edit; can be <code>null</code>
		 * @param    value        the value of the cell to be edited; it is
		 * up to the specific editor to interpret
		 * and draw the value.  For example, if value is
		 * the string "true", it could be rendered as a
		 * string or it could be rendered as a check
		 * box that is checked.  <code>null</code>
		 * is a valid value
		 * @param    isSelected    true if the cell is to be rendered with
		 * highlighting
		 * @param    row the row of the cell being edited
		 * @param    column the column of the cell being edited
		 * @return the component for editing
		 */
		@Override
		public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {
			combo.setBackground(table.getBackground());
			combo.setForeground(table.getForeground());
			combo.setSelectedItem(getDrawComponent(value));
			return combo;
		}

		/**
		 * Returns the value contained in the editor.
		 *
		 * @return the value contained in the editor
		 */
		@Override
		public Object getCellEditorValue() {
			if (combo.getSelectedItem() != null) {
				return ((LabelRenderer) combo.getSelectedItem()).getKey();
			} else {
				return null;
			}
		}
	}
}
