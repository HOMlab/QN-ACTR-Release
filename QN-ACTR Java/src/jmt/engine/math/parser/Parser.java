/**    
  * Copyright 2006 Bertoli Marco

  *  Licensed under the Apache License, Version 2.0 (the "License");
  *  you may not use this file except in compliance with the License.
  *  You may obtain a copy of the License at

  *  http://www.apache.org/licenses/LICENSE-2.0

  *  Unless required by applicable law or agreed to in writing, software
  *  distributed under the License is distributed on an "AS IS" BASIS,
  *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  *  See the License for the specific language governing permissions and
  *  limitations under the License.
  */
package jmt.engine.math.parser;

import java.util.HashSet;

/**
 * <p><b>Name:</b> Parser</p> 
 * <p><b>Description:</b> 
 * Simple Java arithmetic expression parser and ELL(1) grammar syntactical analizer and evaluator.
 * Parser supports detection of implicit multiplication when a constant is followed by a variable or function.
 * </p>
 * <p><b>Grammar:</b><br>
 * S -&gt; E (('+' | '-') E)*<br>
 * E -&gt; G ('*' G)*<br>
 * G -&gt; H ('/' H)*<br>
 * H -&gt; T ('%' T)*<br>
 * T -&gt; F | '-' F<br>
 * F -&gt; U ('^' U)*<br>
 * U -&gt; function '(' S ')' | number | '(' S ')' | variable<br>
 * </p>
 * <p><b>Date:</b> 08/dic/06
 * <b>Time:</b> 13:58:28</p>
 * @author Bertoli Marco
 * @version 1.0
 */
public class Parser {
	// Special tokens
	private static final char TERM = '@'; // Input end character
	private static final char NUM = 'n'; // number
	private static final char FUNC = 'f'; // function
	private static final char VAR = 'x'; // variable
	private static final char OP_BRACKET = '('; // open bracket
	private static final char CL_BRACKET = ')'; // closed bracket

	private char[] input;
	private boolean error;
	private int inp_cur; // Current input position
	private Element cc; // Current character
	private ExpressionNode root; // Root of expression tree

	private HashSet<String> functions; // Used functions
	private HashSet<String> variables; // Used variables

	/**
	 * Class constructor
	 * @param str Input string to be analyzed
	 */
	public Parser(String str) {
		this(str, false);
	}

	/**
	 * Class constructor
	 * @param str Input string to be analyzed
	 * @param error if true, an exception is raised when value is get
	 * from a variable not initialized. If false, that variable is
	 * considered as zero.
	 */
	public Parser(String str, boolean error) {
		input = convertInput(str);
		root = null;
		inp_cur = 0;
		this.error = error;
		functions = new HashSet<String>();
		variables = new HashSet<String>();
	}

	/**
	 * Converts input string in a character array and removes unsupported characters
	 * @param str input string
	 * @return converted string, terminated by TERM character
	 */
	protected char[] convertInput(String str) {
		char[] input = str.toCharArray();
		char[] output = new char[input.length + 1]; // One more space for terminator
		int pos = 0; // Position on output
		for (char c : input) {
			if (isNumber(c) || isOperator(c) || isText(c) || isBracket(c)) {
				output[pos++] = c;
			}
		}
		output[pos++] = TERM;
		return output;
	}

	/**
	 * Returns the tree of the function generated by the parser
	 * @return Node root node of parsed tree
	 * @throws ParseError if parsed input string was malformed
	 */
	public ExpressionNode getTree() throws ParseError {
		if (root == null) { // Function need parsing
			cc = parse();
			root = S();
			if (!isEndOfExpression(cc.getToken())) {
				throw new ParseError("Expecting operator or end of input", inp_cur);
			}
			return root;
		} else {
			return root; // Parsing was already performed before
		}
	}

	/**
	 * Returns if input character is a number
	 * @param c input character
	 * @return truth value
	 */
	private boolean isNumber(char c) {
		if ((c >= '0' && c <= '9') || c == '.') {
			return true;
		}
		return false;
	}

	/**
	 * Returns if imput character character is an operator
	 * @param c input character
	 * @return truth value
	 */
	private boolean isOperator(char c) {
		for (char element : OperatorNode.OPERATIONS) {
			if (c == element) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns if input character is valid text
	 * @param c input character
	 * @return truth value
	 */
	private boolean isText(char c) {
		return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_');
	}

	/**
	 * Returns if input character is a bracket
	 * @param c character
	 * @return truth value
	 */
	private boolean isBracket(char c) {
		return (c == OP_BRACKET) || (c == CL_BRACKET);
	}

	/**
	 * Returns if imput character is the last one (TERM)
	 * @param c input character
	 * @return truth value
	 */
	private boolean isEndOfExpression(char c) {
		return c == TERM;
	}

	/**
	 * Perform parsing of input string
	 * @return Element
	 */
	private Element parse() {
		// Detects implicit multiplication
		if (detectImplicitMult()) {
			return new Element('*');
		}
		// Matches operators, brackets and termination character
		if (isOperator(input[inp_cur]) || isBracket(input[inp_cur]) || isEndOfExpression(input[inp_cur])) {
			return new Element(input[inp_cur++]);
		}

		// Matches numbers
		if (isNumber(input[inp_cur])) {
			int tmp = inp_cur++;
			// Finds the end of number
			while (isNumber(input[inp_cur])) {
				inp_cur++;
			}
			// Eventual exponential
			if (Character.toLowerCase(input[inp_cur]) == 'e') {
				if (input[inp_cur + 1] == '-' || isNumber(input[inp_cur + 1])) {
					inp_cur += 2;
				}
				while (isNumber(input[inp_cur])) {
					inp_cur++;
				}
			}
			String s = new String(input, tmp, inp_cur - tmp);
			double d;
			try {
				d = Double.valueOf(s).doubleValue();
			} catch (NumberFormatException ex) {
				throw new ParseError("Invalid number: " + s, inp_cur);
			}
			return new Element(NUM, d);
		}

		// Matches text (functions or variables or built-in constants)
		if (isText(input[inp_cur])) {
			int tmp = inp_cur++;
			// Finds the end of text
			while (isText(input[inp_cur]) || isNumber(input[inp_cur])) {
				inp_cur++;
			}
			String s = new String(input, tmp, inp_cur - tmp);

			String lower = s.toLowerCase();
			// Now searches if this string is a function
			for (int i = 0; i < FunctionNode.FUNCTIONS.length; i++) {
				if (lower.equals(FunctionNode.FUNCTIONS[i])) {
					functions.add(FunctionNode.FUNCTIONS[i]);
					return new Element(FUNC, i);
				}
			}

			// Now searches if this string is a built-in constant
			for (int i = 0; i < ConstantNode.CONSTANTS.length; i++) {
				if (lower.equals(ConstantNode.CONSTANTS[i])) {
					return new Element(NUM, i);
				}
			}

			// String was not a function, so treat it as a variable
			variables.add(s);
			return new Element(VAR, s);
		}
		//At this point everything not recognized is an error
		throw new ParseError("Unrecognized identifier", inp_cur);
	}

	/**
	 * Helper method to detect implicit multiplication
	 * @return true only if inplicit multiplication is detected
	 */
	private boolean detectImplicitMult() {
		if (cc != null && cc.getToken() == NUM) {
			cc = null; // Otherwise we will loop forever
			// Stores old pointer to restore it back
			int old_input = inp_cur;
			Element next = parse();
			// Restores old pointer back
			inp_cur = old_input;
			if (next.getToken() == VAR || next.getToken() == FUNC) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Grammar Axiom S<br>
	 * S -&gt; E (('+' | '-') E)*
	 * @return Node
	 */
	private ExpressionNode S() {
		ExpressionNode current = E();
		while (cc.getToken() == '+' || cc.getToken() == '-') {
			char operator = cc.getToken();
			cc = parse();
			current = new OperatorNode(current, E(), operator);
		}
		return current;
	}

	/**
	 * Non-terminal E<br>
	 * E -&gt; G ('*' G)*<br>
	 * @return Node
	 */
	private ExpressionNode E() {
		ExpressionNode current = G();
		while (cc.getToken() == '*') {
			cc = parse();
			current = new OperatorNode(current, G(), '*');
		}
		return current;
	}

	/**
	 * Non-terminal G<br>
	 * G -&gt; H ('/' H)*<br>
	 * @return Node
	 */
	private ExpressionNode G() {
		ExpressionNode current = H();
		while (cc.getToken() == '/') {
			cc = parse();
			current = new OperatorNode(current, H(), '/');
		}
		return current;
	}

	/**
	 * Non-terminal H<br>
	 * H -&gt; T ('%' T)*<br>
	 * @return Node
	 */
	private ExpressionNode H() {
		ExpressionNode current = T();
		while (cc.getToken() == '%') {
			cc = parse();
			current = new OperatorNode(current, T(), '%');
		}
		return current;
	}

	/**
	 * Non-terminal T<br>
	 * T -&gt; F | '-' F<br>
	 * @return Node
	 */
	private ExpressionNode T() {
		if (cc.getToken() == '-') {
			cc = parse();
			return new FunctionNode(F(), "-");
		}
		return F();
	}

	/**
	 * Non-terminal F<br>
	 * F -&gt; U ('^' U)*
	 * @return Node
	 */
	private ExpressionNode F() {
		ExpressionNode left;
		left = U();
		if (cc.getToken() == '^') {
			cc = parse();
			return new OperatorNode(left, F(), '^');
		}
		return left;
	}

	/**
	 * Non-terminal U<br>
	 * U -&gt; function '(' S ')' | number | '(' S ')' | variable
	 * @return Node
	 */
	private ExpressionNode U() {
		switch (cc.getToken()) {
			case NUM:
				double n = cc.number;
				int constPosition = cc.position; // Position if this is a built-in constant
				cc = parse();
				if (constPosition >= 0) {
					return new ConstantNode(constPosition);
				} else {
					return new ConstantNode(n);
				}
			case VAR:
				String name = cc.name;
				cc = parse();
				return new VariableNode(name, error);
			case OP_BRACKET:
				cc = parse();
				ExpressionNode tmp = S();
				if (cc.getToken() == CL_BRACKET) {
					cc = parse();
					return tmp;
				}
				throw new ParseError("Semantic Error, expected '" + CL_BRACKET + "'", inp_cur);
			case FUNC:
				int function = cc.position;
				cc = parse();
				if (cc.getToken() == OP_BRACKET) {
					cc = parse();
				} else {
					throw new ParseError("Semantic Error, expected '" + OP_BRACKET + "'", inp_cur);
				}
				ExpressionNode tmp2 = S();
				if (cc.getToken() == CL_BRACKET) {
					cc = parse();
				} else {
					throw new ParseError("Semantic Error, expected '" + CL_BRACKET + "'", inp_cur);
				}
				return new FunctionNode(tmp2, function);
		}
		throw new ParseError("Semantic Error, expected function or variable or constant or '('", inp_cur);
	}

	/**
	 * Data structure used internally to return parsed elements.
	 */
	private class Element {
		private char token;
		public double number = Double.NaN;
		public int position = -1;
		public String name = null;

		public Element(char token) {
			this.token = token;
		}

		public Element(char token, double number) {
			this.token = token;
			this.number = number;
		}

		public Element(char token, String name) {
			this.token = token;
			this.name = name;
		}

		public Element(char token, int position) {
			this.token = token;
			this.position = position;
		}

		/**
		 * Returns the token
		 * @return char
		 */
		public char getToken() {
			return token;
		}
	}

	/**
	 * Sets the value for a given variable
	 * @param name name of the variable to be set (case sensitive)
	 * @param value value for the variable
	 * @throws ParseError if parsed input string was malformed
	 */
	public void setVariable(String name, double value) throws ParseError {
		getTree();
		root.setVariable(name, value);
	}

	/**
	 * Returns the value for evaluated expression
	 * @return value of expression
	 * @throws ParseError if parsed input string was malformed
	 * @throws EvaluationException if one variable was not initialized and parser was 
	 * created with <code>error = true</code>
	 * @see #Parser(String, boolean)
	 */
	public double getValue() throws ParseError, EvaluationException {
		getTree();
		return root.getValue();
	}

	/**
	 * Returns a string rappresentation of parsed expression with the right parentesis
	 * @return a string rappresentation of parsed expression with the right parentesis
	 * @throws ParseError if parsed input string was malformed
	 */
	public String getExpression() throws ParseError {
		getTree();
		return root.toString();
	}

	/**
	 * Returns a Set of all functions parsed in input string
	 * @return a set with all parsed functions
	 * @throws ParseError if parsed input string was malformed
	 */
	public HashSet<String> getParsedFunctions() throws ParseError {
		getTree();
		return functions;
	}

	/**
	 * Returns a Set of all variables parsed in input string
	 * @return a set with all parsed variables
	 * @throws ParseError if parsed input string was malformed
	 */
	public HashSet<String> getParsedVariables() throws ParseError {
		getTree();
		return variables;
	}

	/**
	 * Returns input string, without invalid characters
	 * @return input string without invalid characters
	 */
	public String getInputString() {
		StringBuffer output = new StringBuffer();
		for (int i = 0; i < input.length && input[i] != TERM; i++) {
			output.append(input[i]);
		}
		return output.toString();
	}
}
