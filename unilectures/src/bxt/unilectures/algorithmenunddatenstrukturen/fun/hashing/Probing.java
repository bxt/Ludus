package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * Algorithm to separete otherwise equal hash values
 * @author Burny
 */
public interface Probing {
	/**
	 * Get an alternate hash position
	 * @param key The original key
	 * @param i How many spaces are occupied already
	 * @param max How many spaces are available
	 * @return A new position for the key
	 */
	public int getHash(int key, int i, int max);
}
