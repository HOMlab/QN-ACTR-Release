package jmt.engine.jwat.input;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DataMapping extends VariableMapping {
	@Override
	public double convertToDouble(String val) {
		Date d;
		try {
			SimpleDateFormat f = new SimpleDateFormat("dd/MMM/yyyy:HH:mm:ss");
			d = f.parse(val);
		} catch (ParseException e) {
			return -1;
		}

		return d.getTime();
	}

	@Override
	public Object getValue(double number) {
		return new Date((long) number);
	}

	@Override
	public double addNewValue(String value) {
		return convertToDouble(value);
	}

}
