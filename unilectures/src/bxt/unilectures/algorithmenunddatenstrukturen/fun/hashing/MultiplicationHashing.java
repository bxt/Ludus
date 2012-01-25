package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * Hash a key by multiplicating with a fractional value
 * @author Burny
 */
public class MultiplicationHashing implements Hashing {

	/**
	 * Golden ratio is a good factor according to Knuth
	 */
	private static float DEFAULT_FACTOR = 0.618033f;
	
	/**
	 * The multiplicative value for this instance
	 */
	private float factor;
	
	/**
	 * Build with defautl facotr
	 */
	public MultiplicationHashing() {
		this.factor = DEFAULT_FACTOR;
	}
	
	/**
	 * Build with custom factor
	 * @param factor The multiplicative value for this instance
	 */
	public MultiplicationHashing(float factor) {
		this.factor = factor;
	}

	@Override
	public int getHash(int key, int max) {
		return (int) Math.floor( max * ((key*factor)%1) );
	}

}
