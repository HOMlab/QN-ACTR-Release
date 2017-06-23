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

package jmt.gui.jsim.definitions;

import jmt.gui.common.definitions.CommonModel;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 25-lug-2005
 * Time: 14.31.58
 * This is a variant of CommonModel that automatically add source and sink stations to model
 * as the first open class (if defined) is added to the model. If last open class is
 * deleted, source and sink stations are deleted as well. This class is designed for a
 * single-source and -sink model. Reference source for all open classes is this only source.
 * Modified by Bertoli Marco 4-oct-2005 / Fixed bugs while loading from file
 */
public class JSIMModel extends CommonModel {

	private int openClasses = 0;
	Object src, snk; // Pointer to source and sink reference

	//intercept each call for addition of a new class
	@Override
	public Object addClass(String name, int type, int priority, Integer population, Object distribution) {
		if (type == CLASS_TYPE_OPEN) {
			if (openClasses == 0) {
				addSourceAndSink();
			}
			openClasses++;
		}
		Object key = super.addClass(name, type, priority, population, distribution);
		if (type == CLASS_TYPE_OPEN) {
			setClassRefStation(key, src);
		}
		return key;
	}

	/**Adds a new station to the model. Name and type must be specified. If station is a source
	 * or a sink, returns unique ones
	 * @param name: name of the new station
	 * @param type: string representing station type. It's value is contained in
	 * <code>JSIMConstants</code> interface.
	 * @return : key of search for this class*/
	@Override
	public Object addStation(String name, String type) {
		if (type.equals(STATION_TYPE_SOURCE)) {
			if (src == null) {
				addSourceAndSink();
			}
			return src;
		} else if (type.equals(STATION_TYPE_SINK)) {
			if (snk == null) {
				addSourceAndSink();
			}
			return snk;
		} else {
			return super.addStation(name, type);
		}
	}

	//intercept each call for change of class type
	@Override
	public void setClassType(int type, Object classKey) {
		if (getClassType(classKey) != type) {
			if (type == CLASS_TYPE_OPEN) {
				if (openClasses == 0) {
					addSourceAndSink();
				}
				openClasses++;
				this.setClassRefStation(classKey, src);
			} else {
				if (openClasses == 1) {
					deleteSourceAndSink();
				}
				openClasses--;
			}
		}
		super.setClassType(type, classKey);
	}

	//Intercept each call to the deletion of a class
	@Override
	public void deleteClass(Object classKey) {
		if (getClassType(classKey) == CLASS_TYPE_OPEN) {
			if (openClasses == 1) {
				deleteSourceAndSink();
			}
			openClasses--;
		}
		super.deleteClass(classKey);
	}

	//intercept also calls to station deletion to avoid sink and source deletion
	@Override
	public void deleteStation(Object stationKey) {
		String type = getStationType(stationKey);
		if (STATION_TYPE_SOURCE.equals(type) || STATION_TYPE_SINK.equals(type)) {
			return;
		}
		super.deleteStation(stationKey);
	}

	//intercept calls to station type modification as well
	@Override
	public void setStationType(String type, Object stationKey) {
		String currentType = getStationType(stationKey);
		if (STATION_TYPE_SOURCE.equals(currentType) || STATION_TYPE_SINK.equals(currentType)) {
			return;
		}
		super.setStationType(type, stationKey);
	}

	//addition of source and sink for new open and closed classses
	private void addSourceAndSink() {
		src = super.addStation("Source", STATION_TYPE_SOURCE);
		snk = super.addStation("Sink", STATION_TYPE_SINK);
		//put source and sink at the head of the station list
		stationsKeyset.remove(src);
		stationsKeyset.remove(snk);
		stationsKeyset.add(0, snk);
		stationsKeyset.add(0, src);
	}

	//deletion of source and sink if all open classes are removed
	private void deleteSourceAndSink() {
		for (int i = 0; i < stationsKeyset.size(); i++) {
			String currentType = getStationType(stationsKeyset.get(i));
			if (STATION_TYPE_SOURCE.equals(currentType) || STATION_TYPE_SINK.equals(currentType)) {
				super.deleteStation(stationsKeyset.get(i));
				i--;
			}
		}
	}
}
