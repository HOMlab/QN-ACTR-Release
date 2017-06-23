package jmt.engine.jwat.input;

import jmt.engine.jwat.filters.FilterOnVariable;

public class Parameter {
	private boolean[] varSelected;
	private String[] varName;
	private String[] regularExp;
	private String[] tokenExp;
	private int[] varType;
	private int sampleMethod;
	private FilterOnVariable filter;
	private int options[];

	private String[] defaults;
	private String[] replaces;

	//Input Sampling constants
	public static final int ALL_INPUT = 0;
	public static final int INTERVAL_INPUT = 1;
	public static final int RANDOM_INPUT = 2;
	public static final int FILTER_INPUT = 3;

	//Input Variable Type
	public static final int NUMBER = 0;
	public static final int STRING = 1;
	public static final int DATE = 2;

	public Parameter(boolean[] colSel, int[] type, String[] exp, String[] tokenExp, String[] varName, int sampleMeth, FilterOnVariable filter,
			int options[], String[] def, String[] rep) {
		varType = type;
		varSelected = colSel;
		this.varName = varName;
		sampleMethod = sampleMeth;
		this.filter = filter;
		this.options = options;
		this.tokenExp = tokenExp;
		this.defaults = def;
		this.replaces = rep;
		regularExp = exp;
		elaborateExp();
	}

	private void elaborateExp() {
		String endChar;

		for (int i = 0; i < tokenExp.length; i++) {
			if (tokenExp[i] != null) {
				endChar = tokenExp[i].substring(tokenExp[i].length() - 1, tokenExp[i].length());
				tokenExp[i] = elaborateChar(tokenExp[i].substring(0, 1));
				tokenExp[i] = tokenExp[i] + "[^";
				tokenExp[i] = tokenExp[i] + elaborateChar(endChar);
				tokenExp[i] = tokenExp[i] + "]+";
				tokenExp[i] = tokenExp[i] + elaborateChar(endChar);
			}
		}
	}

	private String elaborateChar(String car) {
		String ret = "";

		if (car.equals("[") || car.equals("]") || car.equals(".") || car.equals("(") || car.equals(")")) {
			ret = "\\";
		}

		if (!car.equals(" ")) {
			ret = ret + car;
		} else {
			ret = "\\s";
		}

		return ret;
	}

	public boolean[] getVarSelected() {
		return varSelected;
	}

	public FilterOnVariable getFilter() {
		return filter;
	}

	public int getSampleMethod() {
		return sampleMethod;
	}

	public int[] getOptions() {
		return options;
	}

	public String[] getVarName() {
		return varName;
	}

	public int[] getVarType() {
		return varType;
	}

	public String[] getRegularExp() {
		return regularExp;
	}

	public String[] getDefaults() {
		return defaults;
	}

	public String[] getReplaces() {
		return replaces;
	}

	public int getNumVarSelected() {
		int ret = 0;
		for (boolean element : varSelected) {
			if (element) {
				ret++;
			}
		}
		return ret;
	}

	public String[] getSelName() {
		String[] nm = new String[getNumVarSelected()];
		int j = 0;

		for (int i = 0; i < getNumVar(); i++) {
			if (varSelected[i]) {
				nm[j++] = varName[i];
			}
		}

		return nm;
	}

	public int[] getSelType() {
		int[] tp = new int[getNumVarSelected()];
		int j = 0;

		for (int i = 0; i < getNumVar(); i++) {
			if (varSelected[i]) {
				tp[j++] = varType[i];
			}
		}

		return tp;
	}

	public int getNumVar() {
		return varSelected.length;
	}

	public String[] getSeparator() {
		return tokenExp;
	}

	public void setSampleMethod(int tipo) {
		sampleMethod = tipo;
	}

	public void setFilter(FilterOnVariable filter) {
		this.filter = filter;
	}

	public void setOption(int[] newOpt) {
		options = new int[newOpt.length];
		for (int i = 0; i < newOpt.length; i++) {
			options[i] = newOpt[i];
		}
	}
}
