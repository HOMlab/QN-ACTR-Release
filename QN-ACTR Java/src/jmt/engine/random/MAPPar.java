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

package jmt.engine.random;

import java.util.ArrayList;

import jmt.common.exception.IncorrectDistributionParameterException;

/**
*
* This is the parameter that should be passed to the Hyper Exponential
* distribution.
*
* <br><br>Copyright (c) 2006
* <br>Politecnico di Milano - dipartimento di Elettronica e Informazione
* @author Giuliano Casale
*
*/

public class MAPPar extends AbstractParameter implements Parameter {

	private double mean;
	private double var;

	private int numStates;
	private ArrayList<?> trProbs;
	private ArrayList<?> hlTimes;
	protected ArrayList<ExponentialPar> expParam;

	public MAPPar(String D0, String D1) throws IncorrectDistributionParameterException {

		testParameters();
		this.mean = 0;
		this.var = 0;
		// creates 2 ExponentialPar objects
		numStates = 2;

		expParam = new ArrayList<ExponentialPar>(numStates);
		for (int i = 0; i < numStates; i++) {
			expParam.add(new ExponentialPar(1.0));
		}
	}

	/**
	 * Tests the parameters for the constructor requiring p, lambda1 and lambda2.
	 *
	 * @throws IncorrectDistributionParameterException if p is not betwen zero and one or if lambda1 and labda2 are not both greater than zero.
	 *
	 */
	private void testParameters() throws IncorrectDistributionParameterException {
	}

	/**
	 * It verify if the parameter is correct. For the hyper exponential
	 * distribution, the parameter is right if the mean calculated is gtz,
	 * the variance is gtz, p probability is betwen 0 and 1 and both the lambda value are gtz.
	 *
	 * @return boolean, indicating wether the parameter is correct or not.
	 *
	 */

	@Override
	public boolean check() {
		return true;
	}

	public double getHoldingTime(int curState) {
		return ((Double) hlTimes.get(curState)).doubleValue();
	}

	public double getTransitionProb(int curState, int destState) {
		return ((Double) trProbs.get((curState - 1) * numStates + destState)).doubleValue();
	}

	/**
	 * it returns the parameter of the 1st exponential.
	 * It returns the parameter used to create the first of the exponential
	 * distribution used by the hyper exponential distribution.
	 *
	 * @return exponentialPar with expParam1, the parameter of the 1st exponential distribution.
	 *
	 */

	public ExponentialPar getExpParam(int curState) {
		return expParam.get(curState);
	}

	/**
	 * it returns the mean of the distribution.
	 * It returns the value of the mean of the hyper exponential distribution which
	 * is provided by the user or evaluated according to other data.
	 *
	 * @return double with the mean of the hyper exponential distribution.
	 *
	 */
	public double getMean() {
		return mean;
	}

	/**
	 * it returns the variance of the distribution.
	 * It returns the value of the variance of the hyper exponential distribution which
	 * is provided by the user or evaluated according to other data.
	 *
	 * @return double with the variance of the hyper exponential distribution.
	 *
	 */

	public double getVar() {
		return var;
	}

	public double getR0() {
		// TODO Auto-generated method stub
		return 0;
	}

	public Parameter getExpParam1() {
		// TODO Auto-generated method stub
		return null;
	}

	public double getR1() {
		// TODO Auto-generated method stub
		return 0;
	}

	public Parameter getExpParam2() {
		// TODO Auto-generated method stub
		return null;
	}

} // end HyperExpPar

//%%%%%%%%%%%%%%%%%%%%%%%%%
/*
int BMAP::get_input(FILE* input, int& index)
{
        int err;
        int i, j, k;
        
        err = fscanf(input, "%d %d", &numState, &numBulk);
        
        *//***        INITIALIZATION      ***/
/*
        states = new struct STATE[numState];
        for (i = 0; i < numState; i++) {
            states[i].mean       = 0.0;
            states[i].rand_ind   = index++;
            states[i].rand_trans = index++;
            states[i].during     = 0.0;
            states[i].p          = new double    [(numBulk+1) * numState];
        }
        
        *//***        INPUT               ***/
/*
        for (j = 0; j < numBulk + 1 && err >= 0; j++) {
            for (i = 0; i < numState && err >= 0; i++) {
                for (k = 0; k < numState && err >= 0; k++) {
                    err = fscanf(input, "%lf", &states[i].p[j*numState+k]);
                    if (states[i].p[j*numState+k] < 0.0) states[i].p[j*numState+k] = 0.0;
                    states[i].mean += states[i].p[j*numState+k];
                } // k
            } // i
        } // j

        for (j = 0; j < numBulk + 1 && err >= 0; j++) {
            for (i = 0; i < numState && err >= 0; i++) {
                for (k = 0; k < numState && err >= 0; k++) {
                    states[i].p[j*numState+k] = (states[i].mean == 0) ? 
                                                0 : states[i].p[j*numState+k] / states[i].mean;
                    if (j*numState+k > 0)
                        states[i].p[j*numState+k] += states[i].p[j*numState+k-1];
                } // k
            } // i
        } // j

        print_P();

        printf("err = %d\n", err); fflush(stdout);
        return err;
}


*//*************************************************************************
* PURPOSE:     Generate interarrival time for BMAP
* RETURN:      the number which follows BMAP distribution
          bulk -- number of arrivals
************************************************************************/
/*
double BMAP::gen_interval(int & bulk)
{
    double  interval    = 0.0;
    double  theo_mean   = 0.0;
    double  prob;
    int i;
    
    SelectStream(states[curr_ind].rand_ind);
    theo_mean = states[curr_ind].mean;
    (theo_mean < 0.000001) ? states[curr_ind].during = INF
                        : states[curr_ind].during = Exponential(1/theo_mean);
    
    interval += states[curr_ind].during;
    if (interval == INF) 
        return interval;
    
    SelectStream(states[curr_ind].rand_trans);
    prob = Uniform(0, 1);
    for (i = 0; i < numState*(numBulk+1); i++)
        if (prob <= states[curr_ind].p[i]) break;
    assert(i < numState*(numBulk+1));
    
    bulk= i / numState;
    i   = i % numState;
    curr_ind = i;
    
    if (bulk == 0)
        interval += gen_interval(bulk);
        
    number  ++;
    Welford_alg(mean, svar, interval, number);
    for (i = 0; i < bulk-1; i++) {
        number ++;
        Welford_alg(mean, svar, 0, number);
    }
    return interval;
}


*/
