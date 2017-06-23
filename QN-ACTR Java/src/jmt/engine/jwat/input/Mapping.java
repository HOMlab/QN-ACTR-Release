package jmt.engine.jwat.input;

public class Mapping implements Comparable {
	private double conversion;
	private Object value;

	public Mapping(double trad, Object val) {
		value = val;
		conversion = trad;
	}

	public double getConversion() {
		return conversion;
	}

	public Object getValue() {
		return value;
	}

	public int compareTo(Object arg0) {
		Mapping t = (Mapping) arg0;
		if (conversion < t.conversion) {
			return -1;
		}
		if (conversion > t.conversion) {
			return 1;
		}
		return 0;
	}

	@Override
	public boolean equals(Object arg0) {
		Mapping t = (Mapping) arg0;
		return t.conversion == this.conversion;
	}
}
