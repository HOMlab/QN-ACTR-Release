package jmt.engine.random;

import jmt.engine.random.engine.RandomEngine;

public abstract class AbstractDistribution {

	//TODO:
	/*
	PROBLEMA: NELL'INTERFACCIA C'E' UN GENERICO PARAMETER MA OGNI DISTRIBUZIONE QUANDO
	IMPLEMENTA USA SOLO IL SUO PARAMETRO. QUINDI IN COMPILAZIONE SEGNALA UN ERRORE...
	SOLUZIONI POSSIBILI:

	1. ANCHE NELLE SOTTOCLASSI METTO PARAMETER E POI INTRODUCO UN CONTROLLO DEL TIPO
	{if par istanceof(ExponentialPar) ...}

	2. FISSO IL PARAMETRO DI CIASCUNA DISTRIBUZ UNA VOLTA PER TUTTE QUANDO USO IL COSTRUTTORE
	(es. Parameter exp_par = new ExponentialPar(...);   )

	3. ??

	PROBABILMENTE BISOGNA CAMBIARE I TESTI DELLE ECCEZIONI PER CONTEMPLARE ANCHE IL PASSAGGIO DI UN
	PARAMETRO SBAGLIATO??
	*/

	/**
	 * Represents the random generator of uniformal distributed 32 bits numbers
	 *
	 */
	protected RandomEngine engine;

	/**
	 * This is the constructor. It creates a new abstract distribution.
	 *
	 */
	public AbstractDistribution() {
		engine = RandomEngine.makeDefault();
	}

}
