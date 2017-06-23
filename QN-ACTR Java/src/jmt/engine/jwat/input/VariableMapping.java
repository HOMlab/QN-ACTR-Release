package jmt.engine.jwat.input;

import java.util.ArrayList;

public abstract class VariableMapping {
	public abstract double convertToDouble(String val);

	public Object getValue(double number) {
		return new Integer(valMap.indexOf(new Mapping(number, null)));
	}

	public double addNewValue(String value) {
		//Check if value is already inserted in the mapping.
		for (int i = 0; i < valMap.size(); i++) {
			if (valMap.get(i).getValue().equals(value)) {
				return valMap.get(i).getConversion();
			}
		}

		double trad = convertToDouble(value);
		valMap.add(new Mapping(trad, value));
		return trad;
	}

	public Mapping[] getMappingValue() {
		Mapping[] map = new Mapping[valMap.size()];
		valMap.toArray(map);
		return map;
	}

	public void addNewMapping(double val, String str) {
		valMap.add(new Mapping(val, str));
	}

	ArrayList<Mapping> valMap = new ArrayList<Mapping>();

}
