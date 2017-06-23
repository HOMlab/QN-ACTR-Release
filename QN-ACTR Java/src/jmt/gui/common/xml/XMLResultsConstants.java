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
 * <p>Title: XML Results Constants</p>
 * <p>Description: This interface contains all constants used by <code>XMLResultsReader</code>
 * and <code>XMLResultsWriter</code>.</p>
 * 
 * @author Bertoli Marco
 *         Date: 3-ott-2005
 *         Time: 10.26.55
 */
public interface XMLResultsConstants {
	public static final String ENCODING = "ISO-8859-1";
	public static final String XML_DOCUMENT_XSD = "Results.xsd";
	public static final String XML_DOCUMENT_ROOT = "results";
	public static final String XML_A_ROOT_POLLING = "pollingInterval";

	public static final String XML_E_MEASURE = "measure";
	public static final String XML_A_MEASURE_NAME = "name";
	public static final String XML_A_MEASURE_TYPE = "type";
	public static final String XML_A_MEASURE_CLASS = "referenceClass";
	public static final String XML_A_MEASURE_STATION = "referenceStation";
	public static final String XML_A_MEASURE_FINALVALUE = "finalValue";
	public static final String XML_A_MEASURE_SAMPLES = "analyzedSamples";
	public static final String XML_A_MEASURE_STATE = "state";
	public static final String XML_A_MEASURE_ALPHA = "alpha";
	public static final String XML_A_MEASURE_PRECISION = "precision";
	public static final String XML_A_MEASURE_NODETYPE = "nodeType";

	public static final String XML_E_SAMPLE = "sample";
	public static final String XML_A_SAMPLE_MEAN = "meanValue";
	public static final String XML_A_SAMPLE_UPPERBOUND = "upperBound";
	public static final String XML_A_SAMPLE_LOWERBOUND = "lowerBound";
	public static final String XML_A_SAMPLE_VALIDITY = "validity"; //only for parametri analysis models

	// --- Constants to read SIMmodeloutput compliant files --------------------------------------------
	public static final String XML_DOCUMENT_O_XSD = "SIMmodeloutput.xsd";
	public static final String XML_DOCUMENT_O_ROOT = "solutions";
	public static final String XML_A_ROOT_O_METHOD = "solutionMethod";
	public static final String XML_A_ROOT_O_MODELPATH = "modelDefinitionPath";

	public static final String XML_EO_MEASURE = "measure";
	public static final String XML_AO_MEASURE_TYPE = "measureType";
	public static final String XML_AO_MEASURE_CLASS = "class";
	public static final String XML_AO_MEASURE_STATION = "station";
	public static final String XML_AO_MEASURE_MEAN = "meanValue";
	public static final String XML_AO_MEASURE_LOWER = "lowerLimit";
	public static final String XML_AO_MEASURE_UPPER = "upperLimit";
	public static final String XML_AO_MEASURE_SAMPLES = "analyzedSamples";
	public static final String XML_AO_MEASURE_DISCARDEDSAMPLES = "discardedSamples";
	public static final String XML_AO_MEASURE_SUCCESFUL = "successful";
	public static final String XML_AO_MEASURE_ALPHA = "alfa";
	public static final String XML_AO_MEASURE_PRECISION = "precision";
	public static final String XML_AO_MEASURE_MAXSAMPLES = "maxSamples";
	public static final String XML_AO_MEASURE_NODETYPE = "nodeType";

}
