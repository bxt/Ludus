package bxt.unilectures.fun.concurrent;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Calculate the sum using {@link Executors}
 */
public class SumsPooling implements Runnable {
	
	private static final int JOB_ANZ = 500;
	private static final int THREAD_ANZ = 50;
	
	private static final long COUNT_TO = 10000000L;
	
	/**
	 * @param args
	 */
	public void run() {
		
		ExecutorService executor = Executors.newFixedThreadPool(THREAD_ANZ);
		
		for (int i=0;i<JOB_ANZ;i++) {
			final long count_to = COUNT_TO+i;
			final int myNumber = i;
			Runnable command = new SumRunnable(count_to, myNumber);
			executor.execute(command);
		}
		executor.shutdown(); // run!
		while (!executor.isTerminated());
		if(!SumRunnable.SILENT) System.out.println("We have 000 Threads running");
	}

}
