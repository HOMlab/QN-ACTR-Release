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

package jmt.gui.exact.ld;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;

import jmt.common.exception.ExpressionParseException;
import jmt.framework.data.ArrayUtils;
import jmt.framework.gui.components.JMTDialog;
import jmt.framework.gui.help.HoverHelp;
import jmt.gui.exact.ExactWizard;
import jmt.gui.exact.ld.eval.Evaluator;
import jmt.gui.exact.ld.eval.FastEvaluator;
import jmt.gui.exact.table.ExactTable;
import jmt.gui.exact.table.ExactTableModel;

/**
 * the editor window itself. Does not release the focus and follows the owner around the screen.
 */
public class LDEditingWindow extends JMTDialog {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	/* dummy arrays to keep the tablemodel happy during initialization */
	private double[][] serviceTimes = new double[1][1];
	private String[] classNames = new String[1];

	private HoverHelp help;
	private Frame owner;
	private Point baseloc = new Point();
	private Point offset = new Point();

	private Evaluator eval = new FastEvaluator();
	private LDHelpDialog helpDialog;

	private LDTable ldTable;
	//private JLabel ldTitle;
	private JTextField ldExpression;

	/* the Actions behind the various components */

	public AbstractAction LD_CANCEL = new AbstractAction("Cancel") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent ae) {
			hide();
			ldEditor.editingCanceled();
		}
	};

	public AbstractAction LD_COMMIT = new AbstractAction("OK") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent ae) {
			hide();
			commit();
			ldEditor.editingStopped();
		}
	};

	public AbstractAction COPYDOWN_ACTION = new AbstractAction("Copy down") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D, ActionEvent.CTRL_MASK, false));
			putValue(Action.SHORT_DESCRIPTION, "Copies the value in the focused cell down to all cells in the same column");
		}

		public void actionPerformed(ActionEvent e) {
			ldTable.copyCellDown();
			ldTable.requestFocus();
		}
	};

	public AbstractAction LD_HELP = new AbstractAction("Help") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		public void actionPerformed(ActionEvent e) {
			showHelp();
		}
	};

	public AbstractAction EVALUATE = new AbstractAction("Evaluate") {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		{
			//putValue(Action.SHORT_DESCRIPTION,"Fill in the selected cells by evaluating an expression");
		}

		public void actionPerformed(ActionEvent e) {
			int errLoc = -1;
			boolean error = false;
			String expr = ldExpression.getText().trim().toLowerCase();
			if (!expr.equals("")) {
				try {
					ldTable.fillCells(expr);
				} catch (ExpressionParseException epe) {
					error = true;
					errLoc = epe.getLoc();
					JOptionPane.showMessageDialog(LDEditingWindow.this, epe.getMessage(), "Error parsing expression", JOptionPane.WARNING_MESSAGE);
				}
			}
			if (error) { //focus the textfield
				ldExpression.setCaretPosition((errLoc < 0 ? 0 : errLoc));
				ldExpression.requestFocus();
			} else {
				ldTable.requestFocus();
			}
		}
	};

	/**
	 * adding this to a Window results in the window not releasing the focus
	 */
	private WindowFocusListener focusCaptor = new WindowFocusListener() {
		public void windowGainedFocus(WindowEvent e) {
		}

		public void windowLostFocus(WindowEvent e) {
			requestFocus();
		}
	};

	/**
	 * makes the editing window follow the wizard if it is moved
	 */
	private ComponentListener moveCatcher = new ComponentAdapter() {
		@Override
		public void componentMoved(ComponentEvent e) {
			setBase(owner.getLocation());
		}
	};

	private LDEditor ldEditor;

	LDEditingWindow(Frame owner, LDEditor ldEditor) {
		super(owner, false);
		this.ldEditor = ldEditor;
		if (owner == null) {
			throw new IllegalArgumentException("owner must not be null!");
		}
		setSize(400, 400); // only height is fixed
		//setUndecorated(true);
		//setResizable(false);
		this.owner = owner;
		setTitle("LD Editor");
		help = ((ExactWizard) owner).getHelp();

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);

		/* closing=cancel */
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				LD_CANCEL.actionPerformed(null);
			}
		});

		initComponents();
	}

	/**
	 * create components and lay them out
	 */
	private void initComponents() {

		ldTable = new LDTable();

		//ldTitle=new JLabel("LD editor");
		//ldTitle.setHorizontalAlignment(SwingConstants.CENTER);
		//ldTitle.setHorizontalTextPosition(SwingConstants.CENTER);
		//ldTitle.setFont(new Font("Arial",Font.BOLD|Font.ITALIC,14));

		ldExpression = new JTextField();
		ldExpression.addActionListener(new ActionListener() { // evaluate as the use presses enter
					public void actionPerformed(ActionEvent e) {
						EVALUATE.actionPerformed(e);
					}
				});

		/*JPanel cp=new JPanel();
		cp.setBorder(BorderFactory.createCompoundBorder(BorderFactory.createRaisedBevelBorder(),
		        BorderFactory.createEtchedBorder()));
		getContentPane().add(cp);*/
		Container cp = getContentPane();

		cp.setLayout(new BorderLayout());

		cp.add(makeButtons(), BorderLayout.SOUTH);

		Box vBox = Box.createVerticalBox();
		vBox.add(Box.createVerticalStrut(5));
		JScrollPane ldTablePane = new JScrollPane(ldTable);
		ldTablePane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
		ldTablePane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
		vBox.add(ldTablePane);
		//vBox.add(Box.createVerticalStrut(5));
		vBox.add(makeTools());
		//vBox.add(Box.createVerticalStrut(5));

		Box totalBox = Box.createHorizontalBox();
		totalBox.add(Box.createHorizontalStrut(5));
		totalBox.add(vBox);
		totalBox.add(Box.createHorizontalStrut(5));

		cp.add(totalBox, BorderLayout.CENTER);
		//cp.add(ldTitle,BorderLayout.NORTH);

	}

	/**
	 * @return the button bar
	 */
	JPanel makeButtons() {
		JPanel res = new JPanel();
		JButton commit = new JButton(LD_COMMIT);
		help.addHelp(commit, "Closes the LD editor saving changes");
		res.add(commit);
		JButton cancel = new JButton(LD_CANCEL);
		help.addHelp(cancel, "Closes the LD editor discarding all changes");
		res.add(cancel);
		JButton help_b = new JButton(LD_HELP);
		help.addHelp(help_b, "Shows help for the LD editor");
		res.add(help_b);
		return res;
	}

	/**
	 * @return the editing tools bar
	 */
	JComponent makeTools() {

		Box vbox = Box.createVerticalBox();

		Box box = Box.createHorizontalBox();
		/*JButton copyanchor=new JButton(ldTable.FILL_ACTION);
		help.addHelp(copyanchor,"Copies the value in the focused cell to all selected cells");
		box.add(copyanchor);*/
		/*JButton copydown=new JButton(COPY_DOWN);
		help.addHelp(copydown,"Copies the value in the focused cell down to all cells in the same column");
		box.add(copydown);*/
		help.addHelp(ldExpression, "Select cells, enter an expression and press ENTER or click \"Evaluate\".");
		ldExpression.setMaximumSize(new Dimension(10000, 22));
		box.add(ldExpression);
		JButton evaluate = new JButton(EVALUATE);
		help.addHelp(evaluate, "Click here to fill selected cells with evaluation results.");
		box.add(evaluate);
		box.setBorder(BorderFactory.createEtchedBorder());

		JPanel lPanel = new JPanel();
		lPanel.setLayout(new BorderLayout());
		JLabel helpLabel = new JLabel(
				"<html>Enter the expression to be evaluated, using <b><i>n</i></b> for the number of customers. For syntax details and a list of supported functions click on \"Help\"</html>");
		helpLabel.setHorizontalAlignment(SwingConstants.CENTER);
		lPanel.add(helpLabel);

		vbox.add(lPanel);
		vbox.add(box);

		return vbox;

	}

	/**
	 * Sets all the data for the editing window. The matrix of service times is copied in order to have a "cancel" feature.
	 */
	public void setStatus(String title, String stationName, String[] classNames, double[][] serviceTimes) {
		this.classNames = classNames;
		this.serviceTimes = ArrayUtils.copy2(serviceTimes);
		//ldTitle.setText("Service times for "+stationName);
		setTitle(title);
		ldTable.updateStructure();
	}

	/**
	 * @return the edited matrix of service times
	 */
	public double[][] getServiceTimes() {
		return serviceTimes;
	}

	/**
	 * if there is a cell being edited, accept the changes.
	 */
	private void commit() {
		if (ldTable.isEditing()) {
			ldTable.getCellEditor().stopCellEditing();
		}
	}

	@Override
	public void show() {
		super.show();
		addWindowFocusListener(focusCaptor);
		owner.addComponentListener(moveCatcher);
		/* make sure we have a selected cell */
		ldTable.setSelectedCell(0, 0);
		ldTable.requestFocus();
	}

	@Override
	public void hide() {
		owner.removeComponentListener(moveCatcher);
		removeWindowFocusListener(focusCaptor);
		super.hide();
	}

	public void setOffset(Point offset) {
		this.offset = offset;
		setLocation(baseloc.x + offset.x, baseloc.y + offset.y);
	}

	public void setBase(Point baseloc) {
		this.baseloc = baseloc;
		setLocation(baseloc.x + offset.x, baseloc.y + offset.y);
	}

	private void showHelp() {
		if (helpDialog == null) {
			helpDialog = new LDHelpDialog(this, eval);
		}
		helpDialog.show();
	}

	//************************************************************************

	private class LDTable extends ExactTable {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		LDTable() {
			super(new LDTableModel());
			autoResizeMode = AUTO_RESIZE_OFF;

			setDisplaysScrollLabels(true);
			setMoreColumnsTooltip("Scroll right to see more classes");
			setMoreRowsTooltip("Scroll down to see more stations");
			setColumnSelectionAllowed(true);
			setRowSelectionAllowed(true);
			setClipboardTransferEnabled(true);

			installKeyboardAction(COPYDOWN_ACTION);
			mouseHandler.getMenu().add(COPYDOWN_ACTION);

			help.addHelp(this,
					"Click or drag to select cells; to edit data single-click and start typing. Right-click for a list of available operations");
			help.addHelp(moreColumnsLabel, "There are more classes: scroll right to see them");
			help.addHelp(moreRowsLabel, "There are more rows: scroll down to see them");
			help.addHelp(selectAllButton, "Click to select all cells");
			tableHeader.setToolTipText(null);
			help.addHelp(tableHeader, "Click, SHIFT-click or drag to select columns");
			rowHeader.setToolTipText(null);
			help.addHelp(rowHeader, "Click, SHIFT-click or drag to select rows");

		}

		@Override
		protected void updateActions() {
			super.updateActions();
			COPYDOWN_ACTION.setEnabled(getSelectedColumnCount() > 0);
		}

		/**
		 * Copies the value of the anchor (focused) down to the end of its column
		 */
		void copyCellDown() {
			stopEditing();
			int anchorRow = selectionModel.getAnchorSelectionIndex();
			ListSelectionModel csm = columnModel.getSelectionModel();
			int colFrom = csm.getMinSelectionIndex();
			int colTo = csm.getMaxSelectionIndex();
			LDTableModel dm = (LDTableModel) dataModel;

			for (int i = colFrom; i <= colTo; i++) {
				dm.copyCellDown(anchorRow, i);
			}
			updateRows(anchorRow + 1, getRowCount());
		}

		/**
		 * Fills in selected cells with values obtained evaluating <code>expr</code>
		 * @throws jmt.common.exception.ExpressionParseException if there were errors parsing expr
		 */
		void fillCells(String expr) throws ExpressionParseException {
			stopEditing();
			int rowFrom = selectionModel.getMinSelectionIndex();
			int rowTo = selectionModel.getMaxSelectionIndex();
			ListSelectionModel csm = columnModel.getSelectionModel();
			int colFrom = csm.getMinSelectionIndex();
			int colTo = csm.getMaxSelectionIndex();

			((LDTableModel) dataModel).fillCells(expr, rowFrom, rowTo, colFrom, colTo);

		}

	}

	/**
	 * rows represent customer #, columns classes
	 */
	private class LDTableModel extends ExactTableModel {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		LDTableModel() {
			prototype = new Double(1000);
			rowHeaderPrototype = "99999";
		}

		public int getRowCount() {
			return serviceTimes[0].length;
		}

		public int getColumnCount() {
			return serviceTimes.length;
		}

		@Override
		public String getColumnName(int index) {
			return classNames[index];
		}

		@Override
		protected Object getValueAtImpl(int rowIndex, int columnIndex) {
			return new Double(serviceTimes[columnIndex][rowIndex]);
		}

		@Override
		protected Object getRowName(int rowIndex) {
			return new Integer(rowIndex + 1);
		}

		@Override
		public void setValueAt(Object value, int rowIndex, int columnIndex) {
			try {
				double newVal = Double.parseDouble((String) value);
				if (newVal >= 0) {
					serviceTimes[columnIndex][rowIndex] = newVal;
				}
			} catch (NumberFormatException e) {
			}
		}

		@Override
		public boolean isCellEditable(int rowIndex, int columnIndex) {
			return true;
		}

		@Override
		public void clear(int row, int col) {
			serviceTimes[col][row] = 0;
		}

		/**
		 * Copy the contents of a cell to an area. Works directly on the data set.
		 */
		@Override
		public void copyCellToArea(int sourceRow, int sourceCol, int rowFrom, int rowTo, int colFrom, int colTo) {

			double source = serviceTimes[sourceCol][sourceRow];

			for (int row = rowFrom; row <= rowTo; row++) {
				for (int col = colFrom; col <= colTo; col++) {
					serviceTimes[col][row] = source;
				}
			}
		}

		/**
		 * Copy the contents of a cell down to the end of the column
		 */
		public void copyCellDown(int row, int col) {
			double source = serviceTimes[col][row];

			int maxRow = serviceTimes[col].length;
			for (int r = row + 1; r < maxRow; r++) {
				serviceTimes[col][r] = source;
			}
		}

		/**
		 * Fill in the cells within an area with the value of expr evaluated in their row index
		 * @throws jmt.common.exception.ExpressionParseException if there were errors parsing expr
		 */
		public void fillCells(String expr, int rowFrom, int rowTo, int colFrom, int colTo) throws ExpressionParseException {
			double[] x = new double[rowTo - rowFrom + 1];

			int i, r, c;

			for (r = rowTo + 1, i = x.length - 1; r > rowFrom; r--, i--) {
				x[i] = r;
			}

			double[] y = eval.evaluate(expr, x);

			for (r = rowFrom, i = 0; r <= rowTo; r++, i++) {
				for (c = colFrom; c <= colTo; c++) {
					serviceTimes[c][r] = y[i];
				}
			}

			fireTableRowsUpdated(rowFrom, rowTo);

		}

	}
}
