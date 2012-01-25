package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

/**
 * Separate hash values by incrementing to the next free entry
 * @author Burny
 */
public class LinearProbing implements Probing {

	@Override
	public int getHash(int key, int i, int max) {
		return mod(key + i, max);
	}

}
