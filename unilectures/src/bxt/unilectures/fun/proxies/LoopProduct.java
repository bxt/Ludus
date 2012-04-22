package bxt.unilectures.fun.proxies;

/**
 * Multiply using a loop. 
 */
public class LoopProduct implements Product {

	@Override
	public long mult(long a, long b) {
		boolean negative = a<0;
		long result = 0;
		if(negative) a = -a;
		for(;a>0;a--) result += b;
		return (negative ? -1 : 1) * result;
	}

}
