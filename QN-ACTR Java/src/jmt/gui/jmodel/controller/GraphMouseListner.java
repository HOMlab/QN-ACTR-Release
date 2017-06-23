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

package jmt.gui.jmodel.controller;

import java.awt.Cursor;
import java.awt.event.MouseEvent;

import javax.swing.event.MouseInputListener;

import org.jgraph.graph.BasicMarqueeHandler;
import org.jgraph.graph.CellHandle;
import org.jgraph.graph.CellView;

/**

 * @author Federico Granata
 * Date: 6-giu-2003
 * Time: 14.17.30

 */
public class GraphMouseListner implements MouseInputListener {

	Mediator mediator;

	protected UIState currentState;

	protected UIState connect, delete, insert, paste, select, nothing;

	/** The focused cell under the mousepointer. */
	protected CellView focus;

	/* The cell under the mousepointer. */
	protected CellView cell;

	/* The object that handles mouse operations. */
	protected Object handler;

	/** Handle that we are going to use. */
	protected CellHandle handle;

	/** Marquee that we are going to use. */
	protected BasicMarqueeHandler marquee;

	protected transient Cursor previousCursor = null;

	public GraphMouseListner(Mediator mediator) {
		insert = new InsertState(mediator);
		this.mediator = mediator;
		currentState = nothing = new UIStateDefault(mediator);
		select = new SelectState(mediator, this);
		connect = new ConnectState(mediator, this);
		marquee = new BasicMarqueeHandler();
	}

	/**
	 * Invoked when the mouse button has been clicked (pressed
	 * and released) on a component.
	 */
	public void mouseClicked(MouseEvent e) {
		currentState.handleClick(e);
	}

	/**
	 * Invoked when a mouse button has been pressed on a component.
	 */
	public void mousePressed(MouseEvent e) {
		currentState.handlePress(e);
	}

	/**
	 * Invoked when a mouse button has been released on a component.
	 */
	public void mouseReleased(MouseEvent e) {
		currentState.handleRelease(e);
	}

	/**
	 * Invoked when the mouse enters a component.
	 */
	public void mouseEntered(MouseEvent e) {
		currentState.handleEnter(e);
	}

	/**
	 * Invoked when the mouse exits a component.
	 */
	public void mouseExited(MouseEvent e) {
		currentState.handleExit(e);
	}

	/**
	 * Invoked when a mouse button is pressed on a component and then
	 * dragged.  <code>MOUSE_DRAGGED</code> events will continue to be
	 * delivered to the component where the drag originated until the
	 * mouse button is released (regardless of whether the mouse position
	 * is within the bounds of the component).
	 * <p>
	 * Due to platform-dependent Drag&Drop implementations,
	 * <code>MOUSE_DRAGGED</code> events may not be delivered during a native
	 * Drag&Drop operation.
	 */
	public void mouseDragged(MouseEvent e) {
		currentState.handleDrag(e);
	}

	/**
	 * Invoked when the mouse cursor has been moved onto a component
	 * but no buttons have been pushed.
	 */
	public void mouseMoved(MouseEvent e) {
		currentState.handleMove(e);
	}

	public void setConnectState() {
		currentState = connect;
	}

	public void setDeleteState() {
		currentState = delete;
	}

	public void setDefaultState() {
		currentState = nothing;
	}

	public void setInsertState(String className) {
		currentState = insert;
		((InsertState) insert).setInsertClass(className);
	}

	public void setPasteState() {
		currentState = paste;
	}

	public void setSelectState() {
		currentState = select;
	}

	/**
	 * Invoked after a cell has been selected in the mouseReleased method.
	 * This can be used to do something interesting if the cell was already
	 * selected, in which case this implementation selects the parent.
	 * Override if you want different behaviour, such as start editing.
	 */
	protected void postProcessSelection(MouseEvent e, Object cell, boolean wasSelected) {
		if (wasSelected && mediator.isCellSelected(cell)) {
			Object parent = cell;
			Object nextParent = null;
			while (((nextParent = mediator.getParent(parent)) != null) && mediator.isCellVisible(nextParent)) {
				parent = nextParent;
			}
			mediator.selectCellForEvent(parent, e);
			focus = mediator.getViewOfCell(parent, false);
		}
	}

	protected boolean isDescendant(CellView parentView, CellView childView) {
		if (parentView == null || childView == null) {
			return false;
		}

		Object parent = parentView.getCell();
		Object child = childView.getCell();
		Object ancestor = child;

		do {
			if (ancestor == parent) {
				return true;
			}
		} while ((ancestor = mediator.getParent(ancestor)) != null);

		return false;
	}

	public void setHandle(CellHandle handle) {
		this.handle = handle;
	}

	public CellHandle getHandle() {
		return handle;
	}

	public CellView getFocus() {
		return focus;
	}

	public Object getHandler() {
		return handler;
	}

	public BasicMarqueeHandler getMarquee() {
		return marquee;
	}

	public Cursor getPreviousCursor() {
		return previousCursor;
	}

	public CellView getCell() {
		return cell;
	}

	public void setHandler(Object handler) {
		this.handler = handler;
	}

	public void setFocus(CellView focus) {
		this.focus = focus;
	}

	public void setCell(CellView cell) {
		this.cell = cell;
	}

	protected void handleEditTrigger(Object cell) {
		mediator.startEditingAtCell(cell);
	}

	public UIState getCurrentState() {
		return currentState;
	}

	public void setPreviousCursor(Cursor cursor) {
		previousCursor = cursor;
	}

}
