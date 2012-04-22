package bxt.unilectures.fun.proxies;

/**
 * Multiply using java operator. 
 */
public class NormalProduct implements Product {

	@Override
	public long mult(long a, long b) {
		return a*b;
	}

}
