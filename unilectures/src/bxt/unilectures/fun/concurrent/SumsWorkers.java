package bxt.unilectures.fun.concurrent;

import java.util.ArrayList;
import java.util.List;

public class SumsWorkers implements Runnable {
	
	private static final int THREAD_ANZ = 500;
	
	private static final long COUNT_TO = 10000000L;
	
	/**
	 * @param args
	 */
	@Override
	public void run() {
		List<Thread> threads = new ArrayList<Thread>(THREAD_ANZ);
		for (int i=0;i<THREAD_ANZ;i++) {
			long count_to = COUNT_TO+i;
			int myNumber = i;
			Thread worker= new Thread(new SumRunnable(count_to, myNumber));
			worker.setName(String.valueOf(myNumber));
			worker.start();
			threads.add(worker);
		}
		int running=0;
		do {
			running=0;
			for (Thread thread : threads) {
				if (thread.isAlive()) running++;
			}
			if(!SumRunnable.SILENT) System.out.println(String.format("We have %03d Threads running",running));
		} while (running>0);
	}

}
