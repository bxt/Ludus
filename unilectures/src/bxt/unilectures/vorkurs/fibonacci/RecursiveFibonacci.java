package bxt.unilectures.vorkurs.fibonacci;

import java.util.Scanner;

/**
 * Algorithm to calculate a fibunacci number recursively
 * @author bxt
 * @date 2011-04-28
 */
public class RecursiveFibonacci {
	
	private static final String greetingText="Welche Fibonacci-Zahl soll berechnet werden?\n";
	private static final String resultText="Die %d-te Fibonacci-Zahl lautet: %d\n";
	
	
	/**
	 * Ask which Fibonacci number to calculate and print it
	 * @param args Command line arguments (not used)
	 */
	public static void main(String[] args) {
		Scanner sc=new Scanner(System.in);
		System.out.print(greetingText);
		int n=sc.nextInt();
		System.out.printf(resultText, n, getFib(n));
	}
	
	/**
	 * Recursive method to calculate a Fibonacci number
	 * @param n Index of the Fibonacci number
	 * @return the n-th Fibonacci number
	 */
	public static int getFib(int n) {
		if(n<=1) return n;
		return getFib(n-1)+getFib(n-2);
	}

}