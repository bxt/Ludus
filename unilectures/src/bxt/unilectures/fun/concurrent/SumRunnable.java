package bxt.unilectures.fun.concurrent;

/**
 * The actual piece of work to be completed 
 */
public final class SumRunnable implements Runnable {
	public static final boolean SILENT=true;
	
	private final long count_to;
	private final int myNumber;

	public SumRunnable(final long count_to,final int myNumber) {
		this.count_to = count_to;
		this.myNumber = myNumber;
	}

	@Override
	public void run() {
		@SuppressWarnings("unused") long sum = 0;
		for(long i=0;i<count_to;i++) {
			sum += i;
		}
		if(!SILENT) System.out.println(String.format("Thread %03d done! Result: %d",myNumber,sum));
	}
}