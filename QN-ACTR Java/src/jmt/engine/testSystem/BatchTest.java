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

package jmt.engine.testSystem;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.log4j.FileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

/**
 * Test engine for simulator.
 * It offers different choices:
 * <ul>
 * <li>more simulation runs of the same model</li>
 * <li>one simulation run for different models</li>
 * <li>test with fixed simulation seed</li>
 * <li>ecc...</li>
 *
 * </ul>
 *
 * @author Stefano
 * @version 7-mar-2005 9.24.30
 */
public class BatchTest {

	//Log4J logger
	private Logger logger;
	//logger level
	private Level loggerLevel = Level.INFO;
	//true if you want to receive extended info about confidence intervals
	private boolean extendedInfo = true;

	/**
	 * Creates a BatchTest object with the corresponding logger
	 * @param level logger level
	 * @param extendedInfo if true, extended info about confidence intervals are logged
	 */
	public BatchTest(Level level, boolean extendedInfo) {

		loggerLevel = level;
		logger = Logger.getLogger(BatchTest.class);
		logger.setLevel(level);

		//for example: 20050307_204315 -> 7 March 2005, 20:43:15
		String datePattern = "yyyyMMdd_HHmmss";
		SimpleDateFormat formatter = new SimpleDateFormat(datePattern);

		Date today = new Date();
		String todayString = formatter.format(today);

		String logName = todayString + "_batch_test.csv";

		try {
			//standard
			//PatternLayout pattern = new PatternLayout("%r [%t] %p %c %x - %m%n");
			//PatternLayout pattern = new PatternLayout("%r - %p %x - %m%n");
			PatternLayout pattern = new PatternLayout("%m%n");

			File logFile = new File(logName);
			if (!logFile.createNewFile()) {
				throw new IOException("trying to overwrite an existing file");
			};

			FileAppender app = new FileAppender(pattern, logName, false);
			logger.addAppender(app);
			logger.setAdditivity(false);

		} catch (IOException e) {
			System.out.println("Error in creating log file");
			return;
		};

	}

	public static void test_fixedPath(String path) {
		BatchTest btest = new BatchTest(Level.INFO, true);
		ResultsCheck check = new ResultsCheck(path, true);
		check.checkRes();
	}

	/**
	 * Executes multiple runs for a set of models.
	 * The seeds used for model generation and for simulation
	 * are automatically generated.
	 * @param classN number of classes for each random model
	 * @param statN number of stations for each random model
	 * @param modelN number of random models
	 * @param modelType type of random models
	 * @param simRuns number of simulation runs for each model
	 */
	public static void comboTest(int classN, int statN, int modelN, int modelType, int simRuns) {

		//debug level = INFO with extended info.
		BatchTest btest = new BatchTest(Level.INFO, true);

		for (int m = 0; m < modelN; m++) {

			//Creates a random model of the specified type
			//uses automatic seed for model generation
			RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, -1);
			File model = randomGen.saveToFile();

			//result check with extended info
			ResultsCheck check = new ResultsCheck(model.getAbsolutePath(), true);
			//multiple runs
			check.checkRes(simRuns);
		}

	}

	/**
	 * Executes multiple runs for one model, generated with the passed seed.
	 * The seed used for simulations is automatically generated.
	 * @param classN number of classes for the random model
	 * @param statN number of stations for the random model
	 * @param modelGenSeed the seed used to generate the random model
	 * @param modelType type of random model
	 * @param simRuns number of simulation runs for the model
	 */
	public static void test_fixedModelSeed(int classN, int statN, long modelGenSeed, int modelType, int simRuns) {

		//debug level = INFO with extended info.
		BatchTest btest = new BatchTest(Level.INFO, true);

		//Creates a random model of the specified type
		//uses passed seed for model generation
		RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, modelGenSeed);
		File model = randomGen.saveToFile();

		//result check with extended info
		ResultsCheck check = new ResultsCheck(model.getAbsolutePath(), true);
		//multiple runs (of course with automatic seed)
		check.checkRes(simRuns);

	}

	/**
	 * Executes a single test with the passed simulation seed. The seeds used
	 * for model generation is automatically generated.
	 * @param classN number of classes for each random model
	 * @param statN number of stations for each random model
	 * @param modelType type of random models
	 * @param simSeed the simulation seed
	 */
	public static void test_fixedSimSeed(int classN, int statN, int modelType, long simSeed) {

		//debug level = INFO with extended info.
		BatchTest btest = new BatchTest(Level.INFO, true);

		//Creates a random model of the specified type
		//uses automatic seed for model generation
		RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, -1);
		File model = randomGen.saveToFile();

		//result check with extended info and fixed seed
		ResultsCheck check = new ResultsCheck(model.getAbsolutePath(), true, simSeed);
		//fixed seed -> only one run
		check.checkRes();
	}

	/**
	 * Executes a single test with the passed seeds for model generation and for simulation.
	 * @param classN number of classes for each random model
	 * @param statN number of stations for each random model
	 * @param modelGenSeed the seed used to generate the random model
	 * @param modelType type of random models
	 * @param simSeed the simulation seed
	 */
	public static void test_fixedSimSeedAndModelSeed(int classN, int statN, long modelGenSeed, int modelType, long simSeed) {

		//debug level = INFO with extended info.
		BatchTest btest = new BatchTest(Level.INFO, true);

		//Creates a random model of the specified type
		//uses the fixed seed for model generation
		RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, modelGenSeed);
		File model = randomGen.saveToFile();

		//result check with extended info and fixed seed
		ResultsCheck check = new ResultsCheck(model.getAbsolutePath(), true, simSeed);
		//fixed seed -> only one run
		check.checkRes();

	}

	/**
	 * Executes a single test with the passed seeds for model generation and for simulation.
	 * Of course all the sim runs will be exactly the same: this method is used only to
	 * test jsim performance.
	 * @param classN number of classes for each random model
	 * @param statN number of stations for each random model
	 * @param modelGenSeed the seed used to generate the random model
	 * @param modelType type of random models
	 * @param runs number of runs
	 * @param simSeed the simulation seed
	 */
	public static void test_fixedSimSeedAndModelSeed_runs(int classN, int statN, long modelGenSeed, int modelType, int runs, long simSeed) {

		//debug level = INFO with extended info.
		BatchTest btest = new BatchTest(Level.INFO, true);

		//Creates a random model of the specified type
		//uses the fixed seed for model generation
		RandomMVAModelGenerator randomGen = new RandomMVAModelGenerator(classN, statN, modelType, modelGenSeed);
		File model = randomGen.saveToFile();

		//result check with extended info and fixed seed
		ResultsCheck check = new ResultsCheck(model.getAbsolutePath(), true, simSeed);
		//fixed seed -> only one run
		check.checkRes(runs);

	}

}
