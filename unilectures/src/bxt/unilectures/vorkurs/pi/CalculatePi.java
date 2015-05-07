package bxt.unilectures.vorkurs.pi;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

/**
 * Calculate Pi using various algorithms
 * @author bxt
 * @date 2011-04-28
 */
public class CalculatePi {
	
	/**
	 * Map of user input integers and corresponding algorithms
	 */
	private static final Map<Integer,Class<? extends PiStrategy>> ALGO_MAP;
	static {
		Map<Integer,Class<? extends PiStrategy>> map=new HashMap<Integer,Class<? extends PiStrategy>>(3);
		map.put(0, LeibnizRowPi.class);
		map.put(1, MonteCarloRandomPi.class);
		map.put(2, MonteCarloRasterPi.class);
		ALGO_MAP=Collections.unmodifiableMap(map);
	}
	
	/**
	 * Ask for algorithm and precision and print out Pi on CLI
	 * @param args Command line arguments (not used)
	 */
	public static void main(String[] args) {
		try (Scanner sc = new Scanner(System.in)) {
			System.out.println("Welcher Algorithmus soll verwendet werden? ");
			for (Integer key:ALGO_MAP.keySet()) {
				System.out.println("  "+key+" => "+ALGO_MAP.get(key).getSimpleName());
			}
			int algoNo=sc.nextInt();
			try {
				PiStrategy calculator=ALGO_MAP.get(algoNo).newInstance();
				System.out.println("Wieviele iterationen? ");
				int iterations=sc.nextInt();
				System.out.println("Pi ist etwa: "+calculator.getPi(iterations));
				System.out.println("Pi ist aber: "+Math.PI);
			} catch (InstantiationException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			}
		}
	}

}
