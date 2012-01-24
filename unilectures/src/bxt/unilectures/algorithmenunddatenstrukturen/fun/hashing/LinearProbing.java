package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

public class LinearProbing implements Probing {

	@Override
	public int getHash(int key, int i, int max) {
		return mod(key + i, max);
	}

}
