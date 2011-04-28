package bxt.unilectures.vorkurs.pi;

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
	private static final Map<Integer,Class<? extends PiStrategy>> algoMap=new HashMap<Integer,Class<? extends PiStrategy>>(2);
	static {
		algoMap.put(Integer.valueOf(0), LeibnizRowPi.class);
		algoMap.put(Integer.valueOf(1), MonteCarloRandomPi.class);
		algoMap.put(Integer.valueOf(2), MonteCarloRasterPi.class);
	}
	
	/**
	 * Ask for algorithm and precision and print out Pi on CLI
	 * @param args Command line arguments (not used)
	 */
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		System.out.println("Welcher Algorithmus soll verwendet werden? ");
		for (Integer key:algoMap.keySet()) {
			System.out.println("  "+key+" => "+algoMap.get(key).getSimpleName());
		}
		int algoNo=sc.nextInt();
		try {
			PiStrategy calculator=algoMap.get(algoNo).newInstance();
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
