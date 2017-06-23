package jmt.engine.jwat.filters;

import java.text.DateFormat;
import java.text.ParseException;
import java.util.Date;

import jmt.engine.jwat.Observation;

public class FilterOnData implements FilterOnVariable {

	Date from, to;
	private int varIndex;

	public FilterOnData(Date from, Date to, int index) {
		this.from = from;
		this.to = to;
		varIndex = index;
	}

	public int getVarIndex() {
		return varIndex;
	}

	public boolean isMatching(String value) {
		Date val = null;
		try {
			val = DateFormat.getDateInstance().parse(value);
		} catch (ParseException e) {
			return false;
		}
		if (val.compareTo(from) < 0) {
			return false;
		}
		if (val.compareTo(to) > 0) {
			return false;
		}
		return true;
	}

	public boolean isMatching(Observation o, int pos) {
		Date val = null;
		val = new Date((long) o.getIndex(varIndex));
		if (val.compareTo(from) < 0) {
			return false;
		}
		if (val.compareTo(to) > 0) {
			return false;
		}
		return true;
	}

}
