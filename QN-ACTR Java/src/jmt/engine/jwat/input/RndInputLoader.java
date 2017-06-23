/**
 * 
 */
package jmt.engine.jwat.input;

import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

import jmt.engine.jwat.MatrixOsservazioni;
import jmt.engine.jwat.Observation;
import jmt.engine.random.engine.MersenneTwister;
import jmt.gui.jwat.JWATConstants;

import org.apache.oro.text.regex.PatternMatcherInput;
import org.apache.oro.text.regex.Perl5Compiler;
import org.apache.oro.text.regex.Perl5Matcher;

/**
 * @author Maevar
 *
 */
public class RndInputLoader extends InputLoader implements JWATConstants {

	/**
	 * @param param
	 * @param fileName
	 * @param map
	 * @param prg
	 */
	public RndInputLoader(Parameter param, String fileName, VariableMapping[] map, ProgressShow prg) throws FileNotFoundException {
		super(param, fileName, map, prg);
	}

	/* (non-Javadoc)
	 * @see jwat.Input.workerLoader#construct()
	 */
	@Override
	public Object construct() {
		int i, maxInd, j, k;
		boolean[] catchVal = new boolean[param.getOptions()[0]];
		boolean[] sel = param.getVarSelected();
		FileWriter w = null;
		PatternMatcherInput input = new PatternMatcherInput("");
		Perl5Compiler myComp = new Perl5Compiler();
		Perl5Matcher myMatch = new Perl5Matcher();
		String line = "";
		String[] regExp = param.getRegularExp();
		String[] separator = param.getSeparator();
		double[] lineValue = new double[param.getNumVarSelected()];
		String[] lineToken = new String[param.getNumVar()];
		String parseToken = null;
		MatrixOsservazioni m = null;

		try {
			w = new FileWriter(LogFileName);
		} catch (IOException e1) {
			e1.printStackTrace();
		}

		try {
			initShow(param.getOptions()[1] + 1);
			maxInd = generateRandom(param.getOptions()[0], param.getOptions()[1], catchVal);
			countObs = 0;
			totalRaw = 0;
			line = reader.readLine();

			for (k = 0; k <= maxInd; k++) {
				if (isCanceled()) {
					try {
						w.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
					valori = null;
					msg = InputMsgAbort;
					return null;
				}

				if (!catchVal[k]) {
					line = reader.readLine();
					continue;
				}
				input.setInput(line);
				totalRaw++;
				countObs++;
				if (totalRaw % getStep() == 0) {
					updateInfos(totalRaw, "<HTML># observations read: " + totalRaw + "<p># errors: " + (totalRaw - countObs) + "</HTML>", false);
				}
				//Legge le stringhe e ne controlla il formato
				for (i = 0; i < param.getNumVar(); i++) {
					//Leggo il token con i separatori (se definiti) qualunque cosa ci sia dentro
					if (separator[i] != null) {
						parseToken = separator[i];
					} else {
						parseToken = regExp[i];
					}

					if (myMatch.contains(input, myComp.compile(parseToken))) {
						if (sel[i]) {
							//Prendo il token
							lineToken[i] = myMatch.getMatch().toString();
							if (separator[i] != null) {
								//	Se il token aveva dei separatori li tolgo
								lineToken[i] = lineToken[i].substring(1, lineToken[i].length() - 1);
								//E prendo quello che mi interessa di cio che rimane
								if (myMatch.contains(lineToken[i], myComp.compile(regExp[i]))) {
									lineToken[i] = myMatch.getMatch().toString();
								} else {
									//Riga sbagliata, decremento il num di oss
									countObs--;

									try {
										w.write("Error in row " + k + " : Element " + i + " is wrong\n");
									} catch (IOException e) {
										e.printStackTrace();
									}
									break;
								}
							}
						}
					} else {
						//Riga sbagliata, decremento il num di oss
						countObs--;
						try {
							w.write("Error in row " + k + " : Line doesn't match format (element " + i + " not found)\n");
						} catch (IOException e) {
							e.printStackTrace();
						}
						break;

					}
				}
				j = 0;
				//Riga Corretta
				if (i == param.getNumVar()) {
					if (input.endOfInput()) {
						//Converte i valori (stringa) letti
						for (i = 0; i < param.getNumVar(); i++) {
							if (sel[i]) {
								if (map[i] == null) {
									lineValue[j++] = Double.parseDouble(lineToken[i]);
								} else {
									lineValue[j++] = map[i].addNewValue(lineToken[i]);
								}
							}
						}
						valori.add(new Observation(lineValue, countObs));
					} else {
						//Riga sbagliata, decremento il num di oss
						countObs--;
						try {
							w.write("Error in row " + k + " : Too many fields\n");
						} catch (IOException e) {
							e.printStackTrace();
						}
					}
				}
				line = reader.readLine();
			}
			try {
				w.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			//Wrong format, no element loaded
			if (valori.size() == 0) {
				closeView();
				valori = null;
				msg = InputMsgAbortWrongFormat;
				return null;
			}

			updateInfos(totalRaw, "Calculating Statistics...", false);
			m = new MatrixOsservazioni(valori.toArray(new Observation[valori.size()]), param.getSelName(), param.getSelType(), map);
			updateInfos(totalRaw + 1, "Done", true);
			return m;

		} catch (Exception e) {
			closeView();
			try {
				w.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
			valori = null;
			msg = InputMsgFail;
			return null;
		} catch (OutOfMemoryError err) {
			closeView();
			try {
				w.close();
			} catch (IOException e1) {
				e1.printStackTrace();
			}
			valori = null;
			msg = "Out of memory. Try with more memory";
			return null;
		}
	}

	private int generateRandom(int numTotal, int numToRead, boolean[] catchVal) {
		int[] rndArray = new int[numTotal];
		int max = 0, i, dimArray, rndNum;
		MersenneTwister mst = new MersenneTwister();

		dimArray = numTotal;
		for (i = 0; i < dimArray; i++) {
			rndArray[i] = i;
		}

		for (i = 0; i < numToRead; i++) {
			rndNum = (int) (mst.nextDouble() * dimArray);
			catchVal[rndArray[rndNum]] = true;
			if (max < rndArray[rndNum]) {
				max = rndArray[rndNum];
			}
			rndArray[rndNum] = rndArray[--dimArray];
		}

		return max;
	}

}
