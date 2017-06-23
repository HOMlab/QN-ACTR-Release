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

package jmt.gui.common.xml;

/**
 * <p>Title: Gui XML Constants</p>
 * <p>Description: Constants used by <code>GuiXMLWriter</code>, <code>GuiXMLReader</code>
 * and <code>XMLArchiver</code> to read and write XML.</p>
 * 
 * @author Bertoli Marco
 *         Date: 22-lug-2005
 *         Time: 18.01.33
 */
public interface GuiXMLConstants {
	public static final String XML_DOCUMENT_XSD = "JModelGUI.xsd";
	public static final String XML_DOCUMENT_ROOT = "jmodel";

	public static final String XML_E_CLASS = "userClass";
	public static final String XML_A_CLASS_NAME = "name";
	public static final String XML_A_CLASS_COLOR = "color";

	public static final String XML_E_STATION = "station";
	public static final String XML_A_STATION_NAME = "name";
	public static final String XML_E_POSITION = "position";
	public static final String XML_A_POSITION_X = "x";
	public static final String XML_A_POSITION_Y = "y";
	public static final String XML_A_POSITION_ROTATE = "rotate";

	public static final String XML_E_PARAMETRIC = "parametric";
	public static final String XML_A_PARAMETRIC_CLASSPATH = "classPath";
	public static final String XML_A_PARAMETRIC_ENABLED = "enabled";
	public static final String XML_E_FIELD = "field";
	public static final String XML_A_FIELD_NAME = "name";
	public static final String XML_A_FIELD_VALUE = "value";

	public static final String XML_ARCHIVE_DOCUMENT_XSD = "Archive.xsd";
	public static final String XML_ARCHIVE_DOCUMENT_ROOT = "archive";

	public static final String XML_ARCHIVE_A_NAME = "name";
	public static final String XML_ARCHIVE_A_TIMESTAMP = "timestamp";

	public static final String ENCODING = "ISO-8859-1";
}
