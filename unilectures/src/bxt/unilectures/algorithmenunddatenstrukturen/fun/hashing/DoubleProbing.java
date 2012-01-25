package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

/**
 * Separate hashes by using another hash value
 * @author Burny
 */
public class DoubleProbing implements Probing {
	
	/**
	 * A hasher to produce the other value with
	 */
	private Hashing hasher;
	
	/**
	 * Build wih a custom hasher
	 * @param hasher For building the other values
	 */
	public DoubleProbing(Hashing hasher) {
		this.hasher=hasher;
	}
	
	@Override
	public int getHash(int key, int i, int max) {
		return mod( key + i*hasher.getHash(key, max) ,max);
	}

}
