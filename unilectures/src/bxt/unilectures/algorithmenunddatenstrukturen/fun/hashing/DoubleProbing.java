package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

public class DoubleProbing implements Probing {

	private Hashing hasher;
	
	public DoubleProbing(Hashing hasher) {
		this.hasher=hasher;
	}
	
	@Override
	public int getHash(int key, int i, int max) {
		return mod( key + i*hasher.getHash(key, max) ,max);
	}

}
