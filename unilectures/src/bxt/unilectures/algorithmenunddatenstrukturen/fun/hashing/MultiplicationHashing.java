package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

public class MultiplicationHashing implements Hashing {

	private static float DEFAULT_FACTOR = 0.618033f;
	
	private float factor;
	
	public MultiplicationHashing() {
		this.factor = DEFAULT_FACTOR;
	}

	public MultiplicationHashing(float factor) {
		this.factor = factor;
	}

	@Override
	public int getHash(int key, int max) {
		return (int) Math.floor( max * ((key*factor)%1) );
	}

}
