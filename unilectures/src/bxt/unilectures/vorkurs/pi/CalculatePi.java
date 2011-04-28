package bxt.unilectures.vorkurs.pi;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class CalculatePi {
	
	private static final Map<Integer,Class<? extends piStrategy>> algoMap=new HashMap<Integer,Class<? extends piStrategy>>(2);
	static {
		algoMap.put(Integer.valueOf(0), LeibnizRowPi.class);
		algoMap.put(Integer.valueOf(1), MonteCarloPi.class);
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		System.out.println("Welcher Algorithmus soll verwendet werden? ");
		for (Integer key:algoMap.keySet()) {
			System.out.println("  "+key+" => "+algoMap.get(key).getSimpleName());
		}
		int algoNo=sc.nextInt();
		try {
			piStrategy calculator=algoMap.get(algoNo).newInstance();
			System.out.println("Wieviele iterationen? ");
			int iterations=sc.nextInt();
			System.out.println("Pi ist etwa: "+calculator.getPi(iterations));
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		}
	}

}
