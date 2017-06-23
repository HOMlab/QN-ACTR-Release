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

package jmt.common.xml.resources;

import java.io.InputStream;

/**
 * Loads the xsd schemas.
 *
 * @author Modified 12/09/2005 by Bertoli Marco
 * to debug loading of files inside JARS
 */
public class XSDSchemaLoader {

	//******************XSD SCHEMAS**************************//

	//TODO: attenzione ai path

	public static final String JMVA_MODEL_DEFINITION = "JMTmodel.xsd";

	public static final String JSIM_MODEL_DEFINITION = "SIMmodeldefinition.xsd";

	public static final String JSIM_MODEL_RESULTS = "SIMmodeloutput.xsd";

	public static final String JMT_ARCHIVE = "Archive.xsd";

	public static final String JMODEL_GUI_DEFINITION = "JModelGUI.xsd";

	public static final String JSIM_GUI_RESULTS = "Results.xsd";

	//******************XSLT TRANSLATORS**************************//

	public static final String JMVA_TO_JSIM = "MVAtoSIM_parallel.xslt";
	public static final String JSIM_TO_JMVA = "translatorSIMtoMVA.xslt";
	public static final String OUT_TO_JMVA = "OUTtoMVA.xslt";
	public static final String OUT_TO_JMVA_SCALING = "OUTtoMVAscaling.xslt";

	/**  Loads the schema from this directory.
	 * (please put all files in the class package).
	 *
	 * @param schemaName string containing the schema name
	 * @return the path of the schema
	 */
	public static String loadSchema(String schemaName) {

		java.net.URL schemaURL = XSDSchemaLoader.class.getResource(schemaName);

		if (schemaURL == null) {
			return null;
		} else {
			return schemaURL.toExternalForm();
		}
	}

	public static InputStream loadSchemaAsStream(String schemaName) {

		InputStream schemaURLStream = XSDSchemaLoader.class.getResourceAsStream(schemaName);

		if (schemaURLStream == null) {
			return null;
		} else {
			return schemaURLStream;
		}
	}

}
