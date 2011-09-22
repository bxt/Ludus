package bxt.unilectures.fun.concurrent;

public class Benchmark {
	public void benchmark(Runnable task, String taskName) {
		long start = System.currentTimeMillis();
		task.run();
		long end = System.currentTimeMillis();
		System.out.println(String.format("%-15s benötigte %6d ms.",taskName,end-start));
	}
}
