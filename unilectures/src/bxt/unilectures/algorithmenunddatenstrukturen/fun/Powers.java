package bxt.unilectures.algorithmenunddatenstrukturen.fun;

/**
 * Testing different implementations of the integer pow method
 */
public class Powers {
	
	public static interface Power {
		public long pow(long x, long y);
	}
	
	/**
	 * Using Java's Math.pow
	 */
	public static class JavaFloats implements Power {
		@Override public long pow(long x, long y) {
			return (long)Math.pow(x, y);
		}
		
	}
	
	/**
	 * By just multiplying y times
	 */
	public static class Naive implements Power {
		@Override public long pow(long x, long y) {
			long returnValue = 1;
			for (long i=0;i<y;i++) {
				returnValue = returnValue * x;
			}
			return returnValue;
		}
		
	}
	
	/**
	 * Recursively squaring
	 */
	public static class Recursive implements Power {
		@Override public long pow(long x, long y) {
			if(y == 0) return 1;
			long powY2 = pow(x, y/2);
			if (y%2 == 0) return powY2 * powY2;
			return powY2 * powY2 * x;
		}
		
	}
	
	/**
	 * Squaring by binary pattern
	 */
	public static class Binary implements Power {
		@Override public long pow(long x, long y) {
			long yH=y/2,p=1,res=1;
			while(p<=yH) p=p*2;
			while (p>0) {
				res=res*res;
				if(y>=p) {
					y-=p;
					res=res*res*x;
				}
				p=p/2;
			}
			return res;
		}
		
	}
	
	/**
	 * Same as {@link Binary}, but with bitwise operators
	 */
	public static class BinaryBitwise implements Power {
		@Override public long pow(long x, long y) {
			long yH=y >> 1,p=1,res=1;
			if(yH>0xffffffffL) p=1<<32;
			if(yH>0xffffffffffffL) p=1<<48;
			while(p<=yH) p=p<<1;
			while (p>0) {
				res=res*res;
				if(y>=p) {
					y-=p;
					res=res*res*x;
				}
				p=p>>1;
			}
			return res;
		}
		
	}
	
	/**
	 * Main methods to compare speeds
	 * @param args CLI
	 */
	public static void main(String... args) {
		final long COUNT_BASE=20;
		final long COUNT_EXP=12;
		final long COUNT=50000;
		Power[] powers = {
				new JavaFloats(),
				new Naive(),
				new Recursive(),
				new Binary(),
				new BinaryBitwise()
				};
		for (int i=0; i< powers.length; i++) {
			Power p=powers[i];
			long startTime = System.currentTimeMillis();
			for (int n=0; n< COUNT; n++) {
				for (long j=0; j< COUNT_BASE; j++) {
					for (long k=0; k< COUNT_EXP; k++) {
						//System.out.println(j+"^"+k+"="+powers[i].pow(j, k));
						/*if(p.pow(j, k)!=(long)Math.pow(j,k)) {
							throw new RuntimeException("Pow incorrect!");
						}*/
						p.pow(j, k);
					}
				}
			}
			long stopTime = System.currentTimeMillis();
			long runTime = stopTime - startTime;
			System.out.println(powers[i].getClass().getSimpleName()+":"+runTime);
		}
	}
	
}
