package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

/**
 * Use a 2 degree polynome for spreading hash values
 * @author Burny
 */
public class SquareProbing implements Probing {
	
	/**
	 * Coefficient for linear
	 */
	private int c0 = 1;
	/**
	 * Coefficient for squared
	 */
	private int c1 = 1;
	
	/**
	 * Default hash function with c0=c1=1
	 */
	public SquareProbing() {
	}
	
	/**
	 * Construct a custom hash function
	 * @param c0 Coefficient for linear
	 * @param c1 Coefficient for squared
	 */
	public SquareProbing(int c0, int c1) {
		this.c0=c0;
		this.c1=c1;
	}
	
	@Override
	public int getHash(int key, int i, int max) {
		return mod(key + c0*i + c1*i*i, max);
	}

}
