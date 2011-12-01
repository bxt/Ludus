package bxt.unilectures.fun.concurrent;

/**
 * Calculate the sums in current thread only
 */
public class SumsConsecutive implements Runnable {
	
	private static final int JOB_ANZ = 500;
	
	private static final long COUNT_TO = 10000000L;
	
	/**
	 * @param args
	 */
	@Override
	public void run() {
		
		for (int i=0;i<JOB_ANZ;i++) {
			final long count_to = COUNT_TO+i;
			final int myNumber = i;
			Runnable command = new SumRunnable(count_to, myNumber);
			command.run();
		}
		if(!SumRunnable.SILENT) System.out.println("We have run everything!");
	}

}
