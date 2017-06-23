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

package jmt.framework.data;

import java.util.Map;

/**
 * Created by IntelliJ IDEA.
 * User: orsotronIII
 * Date: 28-giu-2005
 * Time: 17.05.47
 * To change this template use Options | File Templates.
 */
public class CachedBDMap extends BDMapImpl {

	//currently stored row and column
	private Map mruXMap, mruYMap;

	private Object mruXYMatch;

	//currently stored keys
	private Object mruXKey, mruYKey;

	@Override
	public Object get(Object xKey, Object yKey) {
		/*        System.out.println("yKey = "+yKey+"; mruYKey ="+mruYKey+" yMap="+mruYMap);
		        System.out.println("xKey = "+xKey+"; mruXKey ="+mruXKey+" xMap="+mruXMap);
		*/if (xKey != null && yKey != null) {
			//if both x and y keys match given keys
			if (xKey.equals(mruXKey) && yKey.equals(mruYKey)) {
				return mruXYMatch;
			} else if (xKey.equals(mruXKey)) {
				//                System.out.println("xMatch");
				//if only x or y keys match current key, return matching found inside proper
				//map.
				return mruXMap.get(yKey);
			} else if (yKey.equals(mruYKey)) {
				//                System.out.println("yMatch");
				return mruYMap.get(xKey);
			} else {
				//                System.out.println("no match");
				//no matching found. Refresh all matchings.
				mruXKey = xKey;
				mruYKey = yKey;
				mruXMap = super.get(mruXKey, BDMap.X);
				mruYMap = super.get(mruYKey, BDMap.Y);
				mruXYMatch = super.get(mruXKey, mruYKey);
				return mruXYMatch;
			}
		} else {
			return super.get(xKey, yKey);
		}
	}

	@Override
	public Map get(Object key, int coordName) {
		if (key != null) {
			switch (coordName) {
				case BDMap.X: {
					if (key.equals(mruXKey)) {
						return mruXMap;
					} else {
						mruXKey = key;
						mruXMap = super.get(mruXKey, coordName);
						mruXYMatch = super.get(mruXKey, mruYKey);
						return mruXMap;
					}
				}
				case BDMap.Y: {
					if (key.equals(mruYKey)) {
						return mruYMap;
					} else {
						mruYKey = key;
						mruYMap = super.get(mruYKey, coordName);
						mruXYMatch = super.get(mruXKey, mruYKey);
						return mruYMap;
					}
				}
			}
		}
		return super.get(key, coordName);
	}

	@Override
	public void put(Object xKey, Object yKey, Object value) {
		if (xKey != null && yKey != null) {
			if (xKey.equals(mruXKey) && yKey.equals(mruYKey)) {
				mruXYMatch = value;
			}
			if (xKey.equals(mruXKey)) {
				mruXMap.put(yKey, value);
			}
			if (yKey.equals(mruYKey)) {
				mruYMap.put(xKey, value);
			}
		}
		super.put(xKey, yKey, value);
	}

	@Override
	public void put(Object key, int coordName, Map newMap) {
		if (key != null) {
			switch (coordName) {
				case BDMap.X: {
					if (key.equals(mruXKey)) {
						mruXMap = newMap;
					}
				}
				case BDMap.Y: {
					if (key.equals(mruYKey)) {
						mruYMap = newMap;
					}
				}
			}
		}
		super.put(key, coordName, newMap);
	}

}
