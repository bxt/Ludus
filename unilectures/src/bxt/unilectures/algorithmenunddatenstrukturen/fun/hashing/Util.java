package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * Utility functions for the hash methods
 * @author Burny
 */
public abstract class Util {
	/**
	 * Calculate the modulus as know from mathematics, i.e. mapping
	 * negative values to a positive number too
	 * @param n Number to cut
	 * @param m Modulus base
	 * @return Rest class 0..(m-1) the n belongs to. 
	 */
	public static int mod(int n, int m) {
		return ((n%m)+m)%m;
	}
}
