package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing.Util.mod;

public class SquareProbing implements Probing {

	private int c0 = 1;
	private int c1 = 1;
	
	public SquareProbing() {
	}
	
	public SquareProbing(int c0, int c1) {
		this.c0=c0;
		this.c1=c1;
	}
	
	@Override
	public int getHash(int key, int i, int max) {
		return mod(key + c0*i + c1*i*i, max);
	}

}
