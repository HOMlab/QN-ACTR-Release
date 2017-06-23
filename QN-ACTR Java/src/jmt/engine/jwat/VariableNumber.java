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

package jmt.engine.jwat;

import java.util.ArrayList;

import jmt.engine.jwat.filters.FilterOnVariable;
import jmt.engine.jwat.input.VariableMapping;
import jmt.engine.jwat.workloadAnalysis.exceptions.TrasformException;
import jmt.gui.jwat.JWATConstants;

/**
 * Description: This class represents a single variable which contains numerical
 * values. It provides statistics information and variable transofrmations and
 * sampling operations
 * 
 * @author Brambilla Davide Matr 667986, Fumagalli Claudio 667971 
 * Created: 1-ago-2006 
 * Modified: 27-dec-2006
 * 
 */
public class VariableNumber implements JWATConstants {
	/* Transformation codes */
	public static final short LOGARITHMIC = 0;
	public static final short MINMAX = 1;
	public static final short STDEV = 2;
	public static final short SAMPLING = 3;
	public static final short NONE = 4;
	/* List of observations sorted according to this variable */
	protected Observation[] obsValue;
	/* Original observations value keep as backup for undo sampling transformations */
	protected Observation[] originalValue;
	/* Variable name */
	protected String name;
	/* Index of this variable in the observation */
	protected int nVar;
	/* Type of variable */
	private short varType;
	/* Univariate statistics object */
	private ArrayList<UnivariateStatistics> varUniStatsTransf = new ArrayList<UnivariateStatistics>(); // UnivariateStatistics
	/* Current statistics info */
	private short statsUniCurrentIndexTransf = 0;
	/* List of transformations applied to variable */
	private ArrayList<Integer> listOfTransfs = new ArrayList<Integer>(); //Integer
	/* Dimensions of intervals and intervals */
	private final int intervalGraphSize = 1000;
	private int[] intervalGraph = new int[intervalGraphSize + 1];
	/* Number of observations */
	private int numOss = 0;
	/* Indeicates if it is sampled */
	private boolean sampled = false;
	/*Mapping della varaibile*/
	protected VariableMapping mapping = null;

	/**
	 * Constructor, creates new numerical variable.
	 * @param valObs
	 *            list of observations already sorted by this variable
	 * @param vName
	 *            variable's name
	 * @param pos
	 *            variable's positione in observation
	 * @param varMapping
	 *            mapping object (null)
	 */
	public VariableNumber(Observation[] valObs, String vName, int pos, short type, VariableMapping varMapping) {
		name = vName;
		nVar = pos;
		numOss = valObs.length;
		varType = type;
		originalValue = new Observation[valObs.length];
		// Saving observations
		for (int i = 0; i < numOss; i++) {
			originalValue[i] = valObs[i];
		}
		mapping = varMapping;
		obsValue = originalValue;
		// Initializes remaining parameters
		varUniStatsTransf.add(new UnivariateStatistics(obsValue, nVar));
		calculateIntervals();
	}

	public VariableMapping getMapping() {
		return mapping;
	}

	/**
	 * Returns Index-th value of the variable
	 * @param Index
	 *            index of variable's value desired
	 * @return value of Index-th value
	 * @throws ArrayIndexOutOfBoundsException
	 *             throws if Index is < 0 or > numbero of observations
	 */
	public double getValue(int Index) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return obsValue[Index].getIndex(nVar);
		}
	}

	/**
	 * Returns the index-th value of the var-th variable of the observaion
	 * @param Index
	 *            index of variable's value desired
	 * @param var
	 *            variable index
	 * @return index-th value of the var-th variable
	 * @throws ArrayIndexOutOfBoundsException
	 *             throws if Index is < 0 or > numbero of observations or var <
	 *             0 or > number of variables ( elements of observation )
	 */
	public double getValue(int Index, int var) throws ArrayIndexOutOfBoundsException {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return obsValue[Index].getIndex(var);
		}
	}

	/**
	 * Returns object holding current univariate variable statistics
	 * @return <code>univariateStatistics</code>
	 */
	public UnivariateStatistics getUniStats() {
		return varUniStatsTransf.get(statsUniCurrentIndexTransf);
	}

	/**
	 * Returns element corresponding to the ind quantile
	 * @param ind
	 *            quantile
	 * @return element corresponding to the ind quantile
	 */
	public double getQuantile(int ind) {
		double quant = 0;
		int[] quantili = varUniStatsTransf.get(statsUniCurrentIndexTransf).getQuantili();

		if (this.Size() % 2 == 0) {
			quant = (obsValue[quantili[ind - 1]].getIndex(nVar) + obsValue[quantili[ind - 1] - 1].getIndex(nVar)) / 2;
		} else {
			quant = obsValue[quantili[ind - 1]].getIndex(nVar);
		}
		return quant;
	}

	/**
	 * Returns number of observations
	 * @return number of observations
	 */
	public int Size() {
		return obsValue.length;
	}

	/**
	 * Returns variable's index
	 * @return variable's index
	 */
	public int getIndex() {
		return nVar;
	}

	/**
	 * Return variable's name
	 * @return variable's name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns variable's type
	 * @return variable's type
	 */
	public short getType() {
		return varType;
	}

	// This method calculates index of intervals used to plot graphs
	private void calculateIntervals() {
		intervalGraph[0] = 0;
		int posIntG = 1;
		double range = varUniStatsTransf.get(statsUniCurrentIndexTransf).getRangeValue();
		double min = varUniStatsTransf.get(statsUniCurrentIndexTransf).getMinValue();
		int nInsG = 0;

		for (int i = 0; i < numOss; i++) {
			// Graph interval ( 1000 intervals )
			if (obsValue[i].getIndex(nVar) <= range * (0.001 * posIntG) + min || obsValue[i].getIndex(nVar) > range + min) {
				nInsG++;
			} else {
				if (nInsG == 0) {
					intervalGraph[posIntG] = intervalGraph[posIntG - 1];
				} else {
					intervalGraph[posIntG] = i - 1;
				}
				posIntG++;
				nInsG = 0;
				i--;
			}
			if (posIntG == 1001) {
				break;
			}
		}
		for (; posIntG < 1001; posIntG++) {
			intervalGraph[posIntG] = obsValue.length - 1;
		}
		intervalGraph[1000] = obsValue.length - 1;
	}

	/** ******* TRANSFORMATIONS SECTION ********* */

	/**
	 * Perform one of the transformations
	 * @param index
	 *            transformation to apply
	 * @throws TrasformException
	 *             throws if it isn't possible to apply transformation
	 */
	public void doTransformation(int index) throws TrasformException {
		// Checks if this is a numeric variable otherwise no transformation
		if (varType == NUMERIC) {
			switch (index) {
				case LOGARITHMIC:
					setLogTransformation();
					break;
				case STDEV:
					setStandardTransformation();
					break;
				case MINMAX:
					setMaxMinTransformation();
					break;
			}
		} else {
			throw new TrasformException("This in not a NUMERIC variable");
		}
		calculateIntervals();
	}

	/**
	 * Performs logarithmic transformation 10 based
	 * @throws TrasformException
	 *             if exists al least one value <= 0
	 */
	private void setLogTransformation() throws TrasformException {
		if (isLogTrasformable()) {
			for (int i = 0; i < numOss; i++) {
				obsValue[i].setIndex(nVar, Math.log(obsValue[i].getIndex(nVar)) / Math.log(10));
			}
			addTrasf(LOGARITHMIC);
		} else {
			throw new TrasformException("Logarithmic transformation not allowed");
		}
	}

	/**
	 * Performs min/max transformation (value - minimum) / (maximum - minimum)
	 */
	private void setMaxMinTransformation() {
		for (int i = 0; i < numOss; i++) {
			obsValue[i].setIndex(nVar, (obsValue[i].getIndex(nVar) - varUniStatsTransf.get(statsUniCurrentIndexTransf).getMinValue())
					/ (varUniStatsTransf.get(statsUniCurrentIndexTransf).getRangeValue()));
		}
		addTrasf(MINMAX);
	}

	/**
	 * Performs the following transformation (value - mean) / (standard deviation)
	 */
	private void setStandardTransformation() {
		for (int i = 0; i < numOss; i++) {
			obsValue[i].setIndex(nVar, (obsValue[i].getIndex(nVar) - varUniStatsTransf.get(statsUniCurrentIndexTransf).getMean())
					/ varUniStatsTransf.get(statsUniCurrentIndexTransf).getDevStd());
		}
		addTrasf(STDEV);
	}

	/**
	 * Adds transformations info to variable
	 * @param transf
	 *            transformation applied
	 */
	private void addTrasf(short transf) {
		listOfTransfs.add(new Integer(transf));
		// Stores new statistics after transformation applied
		varUniStatsTransf.add(new UnivariateStatistics(obsValue, nVar));
		statsUniCurrentIndexTransf += 1;
	}

	/**
	 * Undo last transformation applied to variable if exists
	 */
	public boolean undoLastTrasf() {
		if (listOfTransfs.size() > 0) {
			switch (listOfTransfs.get(listOfTransfs.size() - 1).intValue()) {
				case LOGARITHMIC:
					undoLogTrasformation();
					calculateIntervals();
					break;
				case STDEV:
					undoStandardTransformation();
					calculateIntervals();
					break;
				case MINMAX:
					undoMaxMinTransformation();
					calculateIntervals();
					break;
				case SAMPLING:
					undoSampling();
					undoLastTrasf();
					calculateIntervals();
					return true;
			}
		}
		return false;
	}

	/**
	 * Undo a logarithmic transformation
	 */
	private void undoLogTrasformation() {
		// Removes last Log transformation
		for (int i = 0; i < numOss; i++) {
			obsValue[i].setIndex(nVar, Math.pow(10, obsValue[i].getIndex(nVar)));
		}
		listOfTransfs.remove(listOfTransfs.size() - 1);
		// Removes associated statistics
		varUniStatsTransf.remove(varUniStatsTransf.size() - 1);
		statsUniCurrentIndexTransf -= 1;
	}

	/**
	 * Undo min/max transformation (value - minimum) / (maximum - minimum)
	 */
	private void undoMaxMinTransformation() {
		double oldMin = varUniStatsTransf.get(varUniStatsTransf.size() - 2).getMinValue();
		double oldMax = varUniStatsTransf.get(varUniStatsTransf.size() - 2).getMaxValue();

		for (int i = 0; i < numOss; i++) {
			obsValue[i].setIndex(nVar, obsValue[i].getIndex(nVar) * (oldMax - oldMin) + oldMin);
		}
		listOfTransfs.remove(listOfTransfs.size() - 1);
		// Removes associated statistics
		varUniStatsTransf.remove(varUniStatsTransf.size() - 1);
		statsUniCurrentIndexTransf -= 1;
	}

	/**
	 * Undo the following transformation (value - mean) / (standard deviation)
	 */
	private void undoStandardTransformation() {
		double oldDevStd = varUniStatsTransf.get(varUniStatsTransf.size() - 2).getDevStd();
		double oldMean = varUniStatsTransf.get(varUniStatsTransf.size() - 2).getMean();

		for (int i = 0; i < numOss; i++) {
			obsValue[i].setIndex(nVar, (obsValue[i].getIndex(nVar) * oldDevStd) + oldMean);
		}
		listOfTransfs.remove(listOfTransfs.size() - 1);
		// Removes associated statistics
		varUniStatsTransf.remove(varUniStatsTransf.size() - 1);
		statsUniCurrentIndexTransf -= 1;
	}

	/**
	 * Check if logarithmic transformation is applyable to variable
	 * @return If logarithm is applyable
	 */
	private boolean isLogTrasformable() {
		return ((obsValue[0].getIndex(nVar) > 0));
	}

	/**
	 * Returns names of transformations
	 * @return Array of transformation's names
	 */
	public static String[] getTrasfName() {
		String[] ret = { "Logarithmic", "MinMax", "StandardDev" };
		return ret;
	}

	/**
	 * 
	 * @return
	 */
	public String getTrasfStr() {
		String[] trasfName = { "Log", "Mm", "SD" };
		String ret = "";
		int j = 0;
		int i;

		for (i = 0; i < listOfTransfs.size(); i++) {
			if (listOfTransfs.get(i).intValue() != SAMPLING) {
				ret += trasfName[listOfTransfs.get(i).intValue()] + "(";
				j++; //D
			}
		}
		ret += "X";
		for (i = 0; i < j; i++) {
			ret += ")";
		}
		return ret;
	}

	/** ******** METHODS FOR PLOT ********** */
	// Indica la variabile compagna nel plot del grafico
	private int val;
	// Valore di start e end intervallo di visualizzazione variabile
	private int PosS;
	private int PosE;
	// Valori min e max della variabile gemella nel grafico
	private double minV;
	private double maxV;

	public double getDimInt() {
		return varUniStatsTransf.get(statsUniCurrentIndexTransf).getRangeValue() / 1000;
	}

	public int getNumeroOssPerInt() {
		return PosE - PosS;
	}

	public int getStartInt(int j) {
		return intervalGraph[(j - 1) * 10];
	}

	public int getEndInt(int j) {
		return intervalGraph[j * 10];
	}

	/**
	 * Restituisce il valore relativo all'indice pos dell'array degli intervalli
	 * @param Index
	 *            Indice dell'array degli intervalli
	 * @return Restituzione del valore realtivo ad uno limite dell'intervallo
	 */
	public double getIntValue(int Index) {

		return (obsValue[numOss - 1].getIndex(nVar) - obsValue[0].getIndex(nVar)) * 0.01 * Index + obsValue[0].getIndex(nVar);
	}

	/**
	 * Restituisce il valore relativo all'indice pos dell'array degli intervalli
	 * @param Index
	 *            Indice dell'array degli intervalli
	 * @return Restituzione del valore realtivo ad uno limite dell'intervallo
	 */
	public double getIntValueG(int Index) {
		return (obsValue[numOss - 1].getIndex(nVar) - obsValue[0].getIndex(nVar)) * 0.001 * Index + obsValue[0].getIndex(nVar);

	}

	/**
	 * Impostazione per la ricerca dei punti all'interno di un intervallo per il plot
	 * @param min
	 *            Limite inferiore dell'intervallo come indice dell'array degli intervalli
	 * @param max
	 *            Limite superiore dell'intervallo come indice dell'array degli intervalli
	 * @param minv
	 *            Limite inferiore come valore della variabile compagna dello scatter plot
	 * @param maxv
	 *            Limite superiore come valore della variabile compagna dello scatter plot
	 * @param n
	 *            Posizone della variabile compagna nello scatter plot nelle osservazioni
	 */
	public void setRangeIntervallo(int min, int max, double minv, double maxv, int n) {
		// Settaggio delle informazioni per restituizione osservazioni da plottare
		minV = minv;
		maxV = maxv;
		val = n;
		PosS = intervalGraph[min];
		PosE = intervalGraph[max];
	}

	/**
	 * Restituisce il prossimo indice se esiste dell'osservazione da plottare che corrisponde alle restrizioni 
	 * impostate con setRangeQuantili
	 * 
	 * @return indice che soddisfa le condizioni
	 */
	public int getNextInt() throws Exception {
		while (obsValue[PosS].getIndex(val) < minV || obsValue[PosS].getIndex(val) > maxV) {
			PosS++;
			if (PosS > PosE) {
				throw new Exception();
			}
		}
		if (PosS > PosE) {
			throw new Exception();
		}
		return PosS++;

	}

	/**
	 * Restituisce il range tra posizioni di intervalli
	 * @param min
	 *            posizone minima dell'intervallo
	 * @param max
	 *            posizone massima dell'intervallo
	 * @return range tra i due intervalli
	 */
	public double getIntRange(int min, int max) {
		return (obsValue[numOss - 1].getIndex(nVar) - obsValue[0].getIndex(nVar)) * 0.01 * (max - min);
	}

	/**
	 * Restituisce il range tra posizioni di intervalli
	 * @param min
	 *            posizone minima dell'intervallo
	 * @param max
	 *            posizone massima dell'intervallo
	 * @return range tra i due intervalli
	 */
	public double getIntRangeG(int min, int max) {
		return (obsValue[numOss - 1].getIndex(nVar) - obsValue[0].getIndex(nVar)) * 0.001 * (max - min);
	}

	/**
	 * NON UTILIZZATO ERRATO 
	 * Restituisce l'array degli intervalli
	 * @return vettore contenente le posizioni delle variabili che delimitano gli intervalli
	 */
	/*public int[] getInterval() {
		return interval;
	}*/

	public int getIndex(double value) {
		int i = 0;
		for (i = 0; i < intervalGraph.length; i++) {
			if (value <= obsValue[intervalGraph[i]].getIndex(nVar)) {
				return i;
			}
		}
		return i - 1;
	}

	public int getIndexMin(double value) {
		int i = 0;
		for (i = 0; i < intervalGraph.length; i++) {
			if (value <= obsValue[intervalGraph[i]].getIndex(nVar)) {
				return i;
			}
		}
		return i - 1;
	}

	public int getIndexMax(double value) {
		int i = 0;
		for (i = 0; i < intervalGraph.length; i++) {
			if (value < obsValue[intervalGraph[i]].getIndex(nVar)) {
				return i;
			}
		}
		return i - 1;
	}

	public int[] getInterval1000() {
		return intervalGraph;
	}

	/**
	 * Applies filter to variable
	 * 
	 * @param filter
	 * @return
	 */
	public int applySampling(FilterOnVariable filter) {
		int j = 1;

		sampled = true;
		listOfTransfs.add(new Integer(SAMPLING));
		ArrayList<Observation> temp = new ArrayList<Observation>(); //Observation
		for (int i = 0; i < obsValue.length; i++) {
			if (filter.isMatching(obsValue[i], i)) {
				temp.add(obsValue[i]);
				obsValue[i].setID(j++);
			} else {
				obsValue[i].setValid(false);
			}
		}
		obsValue = new Observation[temp.size()];
		temp.toArray(obsValue);

		temp.clear();
		temp = null;

		numOss = obsValue.length;
		varUniStatsTransf.add(new UnivariateStatistics(obsValue, nVar));
		statsUniCurrentIndexTransf += 1;
		calculateIntervals();
		return numOss;
	}

	/**
	 * Sync variable respect with another variable sampling
	 * 
	 * @param size
	 */
	public void updateOnSampling(int size) {
		listOfTransfs.add(new Integer(SAMPLING));
		int i = 0;
		Observation[] temp = new Observation[size];
		// obsValue = new Observation[size];
		for (Observation element : obsValue) {
			if (element.isValid()) {
				temp[i++] = element;
			}
		}
		obsValue = temp;
		temp = null;

		numOss = obsValue.length;
		varUniStatsTransf.add(new UnivariateStatistics(obsValue, nVar));
		statsUniCurrentIndexTransf += 1;
		calculateIntervals();
	}

	public void undoSampling() {
		resetSampling();
		resetValidity();
	}

	private void resetValidity() {
		for (int i = 0; i < originalValue.length; i++) {
			originalValue[i].setValid(true);
			originalValue[i].setID(i);
		}
	}

	public void resetSampling() {
		sampled = false;
		while (listOfTransfs.size() > 0 && listOfTransfs.get(listOfTransfs.size() - 1).intValue() != SAMPLING) {
			undoLastTrasf();
		}
		obsValue = originalValue;
		numOss = obsValue.length;
		listOfTransfs.remove(listOfTransfs.size() - 1);
		varUniStatsTransf.remove(statsUniCurrentIndexTransf);
		statsUniCurrentIndexTransf -= 1;
		// Removes all others SAMPLING in array
		int p = listOfTransfs.size() - 1;
		for (int i = p; i >= 0; i--) {
			if (listOfTransfs.get(i).intValue() == SAMPLING) {
				listOfTransfs.remove(i);
				varUniStatsTransf.remove(i + 1);
				statsUniCurrentIndexTransf -= 1;
			}
		}
		calculateIntervals();
	}

	public boolean isSampled() {
		return sampled;
	}

	public Observation[] getCurObs() {
		return obsValue;
	}

	public int getObsID(int Index) {
		if (Index < 0 || Index > obsValue.length) {
			throw new ArrayIndexOutOfBoundsException();
		} else {
			return obsValue[Index].getID();
		}
	}

	//Update 28/10/2006: + aggiunta funzione per recupero numero trasformazioni esistenti per la variabile
	//					 + spostamento reset della validity a ID in matrixOsservazioni
	//					 + aggiunte funzioni separate per operazione trasformazione variabili per clustering
	//					 ? controllare con Fuma se le statistiche sono necessarie dopo la trasf altrimenti non si ricalcolano
	public int getNumOfTransf() {
		return listOfTransfs.size();
	}

	/* These two functions are used only to transform and antitransform variable 
	 * for the clutering operation.As you can see aonly STDEV and MINMAX tranformations
	 * are allowed and no control on variable's type are performed.
	 */
	public void doClusteringTrasformation(int index) {
		switch (index) {
			case STDEV:
				setStandardTransformation();
				break;
			case MINMAX:
				setMaxMinTransformation();
				break;
			default:
		}
	}

	public void undoClueringTrasformation() {
		switch (listOfTransfs.get(listOfTransfs.size() - 1).intValue()) {
			case STDEV:
				undoStandardTransformation();
				break;
			case MINMAX:
				undoMaxMinTransformation();
				break;
		}
	}
}
