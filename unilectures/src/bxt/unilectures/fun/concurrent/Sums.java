package bxt.unilectures.fun.concurrent;

public class Sums {

	/**
	 * Try and Benchmark some kinds of concurrent Java-code
	 * <p>
	 * Runns {@link SumRunnable} in 3 different ways. 
	 * <h3>Example output:</h3>
	 * <pre>
	 * {@link SumsConsecutive} benötigte  16266 ms.
	 * {@link SumsPooling}     benötigte   8532 ms.
	 * {@link SumsWorkers}     benötigte   8343 ms.
	 * </pre>
	 * @param args <abbr title="Command-line interface">CLI params</abbr>
	 */
	public static void main(String[] args) {
		Runnable[] tasks = {new SumsConsecutive(),new SumsPooling(),new SumsWorkers()};
		Benchmark bench = new Benchmark();
		for(Runnable task : tasks) {
			bench.benchmark(task, task.getClass().getSimpleName());
		}
	}

}
