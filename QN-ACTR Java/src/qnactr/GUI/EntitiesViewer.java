package qnactr.GUI;

//modified from TableFilterDemo.java

/*
  * Copyright (c) 1995, 2008, Oracle and/or its affiliates. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Oracle or the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */ 

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableRowSorter;

import qnactr.objectDesigner.Entity;
import qnactr.sim.QnactrSimulation;

import java.awt.Dimension;
import java.util.Iterator;
import java.util.LinkedList;


import jmt.engine.simEngine.SimSystem;
import jmt.framework.gui.layouts.SpringUtilities;

public class EntitiesViewer extends JPanel {

    private boolean DEBUG = false;
    private JTable table;
    private JTextField filterText;
    
    private JTextField statusText;
    
    private TableRowSorter<MyTableModel> sorter;

    private int filterColumn = 0;
    
    
    public MyTableModel model;
    
    
    public EntitiesViewer() {
        super();
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

        //Create a table with a sorter.
        model = new MyTableModel();
        sorter = new TableRowSorter<MyTableModel>(model);
        table = new JTable(model);
        table.setRowSorter(sorter);
                
        table.setPreferredScrollableViewportSize(new Dimension(900, 400));
        
//        table.getColumnModel().getColumn(0).setPreferredWidth(30);
//      table.getColumnModel().getColumn(1).setPreferredWidth(10); 
//      table.getColumnModel().getColumn(2).setPreferredWidth(10);
//      table.getColumnModel().getColumn(3).setPreferredWidth(10);
//        table.getColumnModel().getColumn(4).setPreferredWidth(300);
//        table.getColumnModel().getColumn(5).setPreferredWidth(30);
        //the remaining space will be divided among all columns
        //TODO, currently has error after each model update.
      
        table.setFillsViewportHeight(true);
        table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        
        //Create the scroll pane and add the table to it.
        JScrollPane scrollPane = new JScrollPane(table);

        //Add the scroll pane to this panel.
        add(scrollPane);

        
        //Create a separate form for filterText and statusText
        SpringLayout layout = new SpringLayout();
        JPanel form = new JPanel(layout);
        final JLabel l1 = new JLabel("Column \"" + table.getColumnName(filterColumn) + "\" uses filter text:", SwingConstants.TRAILING);
        form.add(l1);
        filterText = new JTextField();
        //Whenever filterText changes, invoke newFilter.
        filterText.getDocument().addDocumentListener(
                new DocumentListener() {
                    public void changedUpdate(DocumentEvent e) {
                        newFilter();
                    }
                    public void insertUpdate(DocumentEvent e) {
                        newFilter();
                    }
                    public void removeUpdate(DocumentEvent e) {
                        newFilter();
                    }
                });
        l1.setLabelFor(filterText);
        form.add(filterText);
        JLabel l2 = new JLabel("Status:", SwingConstants.TRAILING);
        form.add(l2);
        statusText = new JTextField();
        l2.setLabelFor(statusText);
        form.add(statusText);       
        SpringUtilities.makeCompactGrid(form, 2, 2, 6, 6, 6, 6);        
        add(form);
        
        
        
        //When selection changes, provide user with row numbers for
        //both view and model.
        table.getSelectionModel().addListSelectionListener(
            new ListSelectionListener() {
              public void valueChanged(ListSelectionEvent event) {
                int viewRow = table.getSelectedRow();
                int viewColumn = table.getSelectedColumn();
                if (viewRow < 0) {
                  //Selection got filtered away.
                  statusText.setText("");
                } else {
                  int modelRow = table.convertRowIndexToModel(viewRow);
                  int modelColumn = table.convertColumnIndexToModel(viewColumn);
                  statusText.setText(  "(Column, Row). View: (" + viewColumn + ", " + viewRow + "). Model: (" + modelColumn + ", " + modelRow + ")."  );
                }
                
                filterColumn = table.convertColumnIndexToModel(viewColumn);
                l1.setText("Column \"" + table.getColumnName(viewColumn) + "\" uses filter text:");
              }
            }
            );
        
        
    }

    /** 
     * Update the row filter regular expression from the expression in
     * the text box.
     */
    private void newFilter() {
        RowFilter<MyTableModel, Object> rf = null;
        //If current expression doesn't parse, don't update.
        try {
            rf = RowFilter.regexFilter(filterText.getText(), filterColumn);
        } catch (java.util.regex.PatternSyntaxException e) {
            return;
        }
        sorter.setRowFilter(rf);
    }




    public class MyTableModel extends AbstractTableModel {
      private String[] columnNames = {"Entity Tag", "From", "To", "Entity Type", "Last Seen" , "Ended", "Event Priority" };      
      private Object[][] data  = {      {"", "", "", "", "", new Boolean(false), "" },         };
      
        public MyTableModel(){
        
        }
        
        public MyTableModel( LinkedList<Entity> entityList){
          if(entityList == null || entityList.size() == 0){
            
            return;
          }
          
          data = new Object[entityList.size()][columnNames.length];
          Iterator<Entity> itrEntities =  entityList.iterator();
          int rowIndex = 0;
          while (itrEntities.hasNext()){
            Entity anEntity = itrEntities.next();
            updateModelDataRow(rowIndex, anEntity);
            rowIndex++;
          }
          
        }
        
        public void updateModelDataRow(int rowIndex, Entity anEntity ){
          
            data[rowIndex][0] = anEntity.Tag; //"Entity Tag"
            data[rowIndex][1] = anEntity.From;
            data[rowIndex][2] = anEntity.To;
            data[rowIndex][3] = anEntity.Entity_Type;
            data[rowIndex][4] = anEntity.currentPlaceHeader.toString() + "_" + anEntity.currentServerNodeLocalName.toString() + "_" + anEntity.currentServerSectionName.toString(); //"Last Seen"
            data[rowIndex][5] = anEntity.Trash;
            data[rowIndex][6] = anEntity.Event_Priority;
        }
        
        public void updateAnEntityInModelData(Entity anEntity){
          for(int rowIndex = 0; rowIndex < data.length; rowIndex++){
            if( (Integer)data[rowIndex][0] == anEntity.Tag ){
              updateModelDataRow(rowIndex, anEntity);
              this.fireTableRowsUpdated(rowIndex, rowIndex) ; //Notifies all listeners that rows in the range [firstRow, lastRow], inclusive, have been updated.
              //this.fireTableDataChanged(); // this will update all the data rows
              break;
            }
          }
          
        }
        
        
        public int getColumnCount() {
            return columnNames.length;
        }

        public int getRowCount() {
            return data.length;
        }

        public String getColumnName(int col) {
            return columnNames[col];
        }

        public Object getValueAt(int row, int col) {
            return data[row][col];
        }

        /*
         * JTable uses this method to determine the default renderer/
         * editor for each cell.  If we didn't implement this method,
         * then the last column would contain text ("true"/"false"),
         * rather than a check box.
         */
        public Class getColumnClass(int c) {
            return getValueAt(0, c).getClass();
        }

        /*
         * Don't need to implement this method unless your table's
         * editable.
         */
        public boolean isCellEditable(int row, int col) {
            //Note that the data/cell address is constant,
            //no matter where the cell appears onscreen.
//            if (col < 2) {
//                return false;
//            } else {
//                return true;
//            }
          return false;
        }

        /*
         * Don't need to implement this method unless your table's
         * data can change.
         */
        public void setValueAt(Object value, int row, int col) {
            if (DEBUG) {
                System.out.println("Setting value at " + row + "," + col
                                   + " to " + value
                                   + " (an instance of "
                                   + value.getClass() + ")");
            }

            data[row][col] = value;
            fireTableCellUpdated(row, col);

            if (DEBUG) {
                System.out.println("New value of data:");
                printDebugData();
            }
        }

        private void printDebugData() {
            int numRows = getRowCount();
            int numCols = getColumnCount();

            for (int i=0; i < numRows; i++) {
                System.out.print("    row " + i + ":");
                for (int j=0; j < numCols; j++) {
                    System.out.print("  " + data[i][j]);
                }
                System.out.println();
            }
            System.out.println("--------------------------");
            
        }
    }
    

    public void updateModelData( LinkedList<Entity> entityList ){
      //TODO will this be too much computation?

        model = new MyTableModel(entityList);
        table.setModel(model);
        //model.data[0][0] = "test";
        
        sorter = new TableRowSorter<MyTableModel>(model);
        
        //table = new JTable(model);
        
        table.setRowSorter(sorter);
        
        
        //if(table.getColumnModel().getColumnCount() >= 5)table.getColumnModel().getColumn(4).setPreferredWidth(300); //TODO will show error
        
        //model.data[0][0] = "test";
        
        //model.fireTableDataChanged();
        //model.fireTableStructureChanged();
        //System.out.println("updateModelData has model.data.length: " + model.data.length);

    }

}