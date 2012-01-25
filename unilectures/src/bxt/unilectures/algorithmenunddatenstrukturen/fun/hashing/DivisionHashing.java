package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

/**
 * Get an initial hash with a simple modulus calculation
 * @author Burny
 *
 */
public class DivisionHashing implements Hashing {

	@Override
	public int getHash(int key, int max) {
		return mod(key,max);
	}

}
