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
package jmt.gui.jwat;

import java.text.DecimalFormat;

public interface JWATConstants {
	// Position of JWAT directories 
	public String absolutePath = "./";
	public static final int BUTTONSIZE = 25;
	//Variable Type constants
	public static final short NUMERIC = 0;
	public static final short STRING = 1;
	public static final short DATE = 2;

	public final static String HTML_FONT_SMALL = "<font size=\"2\">";
	public final static String HTML_FONT_TIT_END_NO_CAPO = "</b></font>";

	public final static DecimalFormat defaultFormat = new DecimalFormat("000.000E0");

	public static final int KMEANS = 0;
	public static final int FUZZYK = 1;
	//Loading constants
	public final static String LogFileName = "LoadingError.log";
	public final static String InputMsgAbort = "Loading aborted by user";
	public final static String InputMsgAbortWrongFormat = "Wrong format, no data match the given pattern";
	public final static String InputMsgFail = "Fatal error in loading data!!";

	public static final int WORKLOAD_INPUT_PANEL = 1;
	public static final int WORKLOAD_BIVARIATE_PANEL = 2;
	public static final int WORKLOAD_CLUSTERING_PANEL = 3;
	public static final int WORKLOAD_INFOCLUSTERING_PANEL = 4;

	public static final int TRAFFIC_INPUT_PANEL = 1;
	public static final int TRAFFIC_EPOCH_PANEL = 2;
	public static final int TRAFFIC_TEXTUAL_PANEL = 3;
	public static final int TRAFFIC_GRAPH_PANEL = 4;
	public static final int TRAFFIC_GRAPHARRIVAL_PANEL = 5;
}
