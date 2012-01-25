package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * Algorithm to get a numeric value in a range for a unbounded number
 * @author Burny
 */
public interface Hashing {
	/**
	 * Calculate a hash position for the key in the target range
	 * @param key The key
	 * @param m The target range maximum
	 * @return A position 0..(m-1)
	 */
	public int getHash(int key, int max);
}
