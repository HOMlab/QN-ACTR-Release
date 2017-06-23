/*
 * MersenneTwister.java
 *
 *
 * Created on 13 novembre 2002, 17.57
 */

package jmt.engine.random.engine;

/** This is a Java version of the Mersenne Twister.
 * Coded by Takuji Nishimura and Makoto Matsu.
 * <br>
 *<br>Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
 *  All rights reserved.
 * <br><br>
 * Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *  <br>
 *    1. Redistributions of source code must retain the above Copyright
 *       notice, this list of conditions and the following disclaimer.
 *  <br>
 *   2. Redistributions in binary form must reproduce the above Copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *  <br>
 *   3. The names of its contributors may not be used to endorse or promote
 *      products derived from this software without specific prior written
 *      permission.
 *   <br><br>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *  <br>
 *  <br>
 * Any feedback is very welcome.<br>
 * http://www.math.keio.ac.jp/matumoto/emt.html<br>
 * email: matumoto@math.keio.ac.jp<br>
 * <br>
 *
 * @author  (translated  in java by) Federico Granata
 */
public class MersenneTwister extends RandomEngine {

	private int mti;
	private int[] mt = new int[N]; /* set initial seeds: N = 624 words */

	/* Period parameters */
	private static final int N = 624;
	private static final int M = 397;

	private static final int MATRIX_A = 0x9908b0df;
	/* constant vector a */
	private static final int UPPER_MASK = 0x80000000;
	/* most significant w-r bits */
	private static final int LOWER_MASK = 0x7fffffff;
	/* least significant r bits */

	/* for tempering */
	private static final int TEMPERING_MASK_B = 0x9d2c5680;
	private static final int TEMPERING_MASK_C = 0xefc60000;

	private static final int mag0 = 0x0;
	private static final int mag1 = MATRIX_A;
	//private static final int[] mag01=new int[] {0x0, MATRIX_A};
	/* mag01[x] = x * MATRIX_A  for x=0,1 */

	public static final int DEFAULT_SEED = 4357;

	/** Creates a new instance of Prova */
	public MersenneTwister() {
		init_genrand((long) (Math.random() * Long.MAX_VALUE));
	}

	/**
	 * Constructor
	 * @param seed seed used to generate the sequence
	 */
	public MersenneTwister(long seed) {
		init_genrand(seed);
	}

	public void setNewSeed(long seed) {
		init_genrand(seed);
	}

	/** initializes mt[N] with a seed */
	void init_genrand(long s) {
		long internal = s & 0xffffffffL;
		mt[0] = (int) internal;
		for (mti = 1; mti < N; mti++) {
			internal = (0x6C078965L * (internal ^ (internal >> 30)) + mti);
			/* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
			/* In the previous versions, MSBs of the seed affect   */
			/* only MSBs of the array mt[].                        */
			/* 2002/01/09 modified by Makoto Matsumoto             */
			mt[mti] = (int) (internal &= 0xffffffffL);
			/* for >32 bit machines */
		}
	}

	/**
	 * initialized by an array with array-length
	 * @param init_key the array for initializing keys
	 * @param key_length array length
	 */
	private void init_by_array(long[] init_key, int key_length) {
		int i, j, k;
		long[] internal = new long[N];

		internal[0] = 0x12BD6AAL & 0xffffffffL;
		for (mti = 1; mti < N; mti++) {
			internal[mti] = (0x6C078965L * (internal[mti - 1] ^ (internal[mti - 1] >> 30)) + mti);
			/* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
			/* In the previous versions, MSBs of the seed affect   */
			/* only MSBs of the array mt[].                        */
			/* 2002/01/09 modified by Makoto Matsumoto             */
			internal[mti] &= 0xffffffffL;
			/* for >32 bit machines */
			//System.out.println(Long.toHexString(mt[mti]));
		}

		i = 1;
		j = 0;
		k = (N > key_length ? N : key_length);
		for (; k > 0; k--) {
			internal[i] = (internal[i] ^ ((internal[i - 1] ^ (internal[i - 1] >>> 30)) * 0x19660DL)) + init_key[j] + j;
			/* non linear */
			internal[i] &= 0xffffffffL;
			/* for WORDSIZE > 32 machines */
			i++;
			j++;
			if (i >= N) {
				internal[0] = internal[N - 1];
				i = 1;
			}
			if (j >= key_length) {
				j = 0;
			}
		}
		for (k = N - 1; k > 0; k--) {
			internal[i] = (internal[i] ^ ((internal[i - 1] ^ (internal[i - 1] >>> 30)) * 0x5D588B65L)) - i; /* non linear */
			internal[i] &= 0xffffffffL;
			/* for WORDSIZE > 32 machines */
			i++;
			if (i >= N) {
				internal[0] = internal[N - 1];
				i = 1;
			}
		}

		for (i = 1; i < N; i++) {
			mt[i] = (int) (internal[i] & 0xffffffffL);
		}

		mt[0] = 0x80000000; /* MSB is 1; assuring non-zero initial array */
	}

	/* generates a random number on [0,0xffffffff]-interval */
	int genrand_int32() {
		int y;

		if (mti >= N) { /* generate N words at one time */
			int kk;
			if (mti == N + 1) {
				init_genrand(0x1571L); /* a default initial seed is used */
			}
			for (kk = 0; kk < N - M; kk++) {
				y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
				mt[kk] = mt[kk + M] ^ (y >>> 1) ^ ((y & 0x1) == 0 ? mag0 : mag1);
			}
			for (; kk < N - 1; kk++) {
				y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
				mt[kk] = mt[kk + (M - N)] ^ (y >>> 1) ^ ((y & 0x1) == 0 ? mag0 : mag1);
			}
			y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
			mt[N - 1] = mt[M - 1] ^ (y >>> 1) ^ ((y & 0x1) == 0 ? mag0 : mag1);

			mti = 0;
		}
		y = mt[mti++];

		/* Tempering */
		y ^= (y >>> 11);
		y ^= (y << 7) & 0x9d2c5680;
		y ^= (y << 15) & 0xefc60000;
		y ^= (y >>> 18);

		return y;
	}

	/* generates a random number on [0,0x7fffffff]-interval */
	private final int genrand_int31() {
		return (genrand_int32() >>> 1);
	}

	/* generates a random number on [0,1]-real-interval */
	private final double genrand_real1() {
		int i = genrand_int32();
		if (i > 0) {
			return i / 4294967295.0;
			/* divided by 2^32-1 */
		} else {
			return ((double) (i & 0x7fffffff) + 0x80000000L) / 4294967295.0;
		}
	}

	/* generates a random number on [0,1)-real-interval */
	private final double genrand_real2() {
		int i = genrand_int32();
		if (i > 0) {
			return i / 4294967296.0;
			/* divided by 2^32 */
		} else {
			return ((double) (i & 0x7fffffff) + 0x80000000L) / 4294967296.0;
		}
	}

	/* generates a random number on (0,1)-real-interval */
	private final double genrand_real3() {
		int i = genrand_int32();
		if (i > 0) {
			return (i + 0.5) / 4294967296.0;
			/* divided by 2^32 */
		} else {
			return (((double) (i & 0x7fffffff) + 0x80000000L) + 0.5) / 4294967296.0;
		}
	}

	/* generates a random number on [0,1) with 53-bit resolution*/
	private final double genrand_res53() {
		long a = genrand_int32() >>> 5, b = genrand_int32() >>> 6;
		return (a * 67108864.0 + b) / 9007199254740992.0;
	}

	/* These real versions are due to Isaku Wada, 2002/01/09 added */

	/**
	 * Returns a 64 bit uniformly distributed random number in the open unit
	 * interval [0.0,1.0) (including 0.0 and excluding 1.0).
	 */
	@Override
	public double nextDouble() {
		return genrand_res53();
	}

	/**
	 *Returns a 32 bit uniformly distributed random number in the closed interval
	 * [Integer.MIN_VALUE,Integer.MAX_VALUE]
	 * (including Integer.MIN_VALUE and  Integer.MAX_VALUE)
	 */
	@Override
	public int nextInt() {
		return genrand_int32();
	}

	/**
	 *Returns a 32 bit uniformly distributed random number in the closed interval
	 * [0,4294967295]
	 * (including 0 and  4294967295)
	 */
	public long nextLong() {
		int i = genrand_int32();
		if (i > 0) {
			return i;
		} else {
			return (i & 0x7fffffff) + 0x80000000L;
		}
	}

	/**
	 *Returns a 64 bit uniformly distributed random number in the closed interval
	 * [Long.MIN_VALUE,Long.MAX_VALUE]
	 * (including Long.MIN_VALUE and  Long.MAX_VALUE)
	 */
	@Override
	public long nextLong64() {
		int i = genrand_int32();
		int j = genrand_int32();
		long n;
		if (i > 0) {
			n = i;
			n <<= 32;
			if (j > 0) {
				return n + j;
			} else {
				return n + (j & 0x7fffffff) + 0x80000000L;
			}
		} else {//i < 0
			n = (i & 0x7fffffff) + 0x80000000L;
			n <<= 32;
			if (j > 0) {
				return n + j;
			} else {
				return n + (j & 0x7fffffff) + 0x80000000L;
			}
		}
	}

	/**
	 * Returns a 32 bit uniformly distributed random number in the open unit
	 * interval (0.0,1.0)  (excluding 0.0 and 1.0).
	 */
	@Override
	public double raw() {
		return genrand_real3();
	}

	/**
	 * test
	 */
	public static void test() {
		int integer;
		MersenneTwister p = new MersenneTwister();
		long init[] = { 0x123L, 0x234L, 0x345L, 0x456L };
		p.init_by_array(init, init.length);

		System.out.println("\nINT:  ");
		/*
		for(int i =0; i< 1000; i++) {
		    integer = p.genrand_int32();
		    if (integer > 0)
		        System.out.print(integer+" ");
		    else
		        System.out.print(((long) (integer & 0x7fffffff) + 0x80000000L) + " ");
		    if(i%5 == 4)
		        System.out.println();
		}*/

		for (int i = 0; i < 1000; i++) {
			System.out.print(p.nextLong() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		System.out.println("\nDOUBLE [0,1):  ");
		for (int i = 0; i < 1000; i++) {
			System.out.print(p.genrand_real2() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		p.init_genrand(723);
		System.out.println("\nINT:  ");
		for (int i = 0; i < 1000; i++) {
			integer = p.genrand_int32();
			if (integer > 0) {
				System.out.print(integer + " ");
			} else {
				System.out.print(((integer & 0x7fffffff) + 0x80000000L) + " ");
			}
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		System.out.println("\nDOUBLE [0,1]:  ");
		for (int i = 0; i < 1000; i++) {
			System.out.print(p.genrand_real1() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		System.out.println("\nDOUBLE [0,1):  ");
		for (int i = 0; i < 1000; i++) {
			System.out.print(p.genrand_real2() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		System.out.println("\nDOUBLE (0,1):  ");
		for (int i = 0; i < 1000; i++) {
			System.out.print(p.genrand_real3() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

		System.out.println("\nDOUBLE 64 [0,1):  ");
		for (int i = 0; i < 1000; i++) {
			System.out.print(p.genrand_res53() + " ");
			if (i % 5 == 4) {
				System.out.println();
			}
		}

	}

}
