package jmt.engine.jwat.input;

public class PositionalMapping extends VariableMapping {

	@Override
	public double convertToDouble(String val) {
		return valMap.size();
	}

}
