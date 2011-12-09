package bxt.unilectures.algorithmenunddatenstrukturen.fun;

import java.util.Stack;

/**
 * Check whether or not an array could have been constructed
 * by pushing the numbers 1..n on a stack and intermediately randomly
 * popping some numbers into the array. 
 */
public class PushAndPop {
	
	/**
	 * Test two arrays and print their possible push/pop construction
	 * steps. 
	 * @param args
	 */
	public static void main(String[] args) {
		int[] seqa={4,3,2,5,1,6,9,8,7,10};
		System.out.println(check(seqa));
		int[] seqb={4,2,1,3,7,6,5,10,9,8};
		System.out.println(check(seqb));
	}
	
	/**
	 * Check if or not this array could have been constructed using the push 
	 * and pop method. Print the necessary push ("+") and pop ("-") steps. 
	 * @param seq The array to check
	 * @return True if the array could have been made by push and pop
	 */
	public static boolean check(int[] seq) {
		Stack<Integer> stack = new Stack<Integer>();
		int maxSeen=0;
		for (int i=0;i<seq.length;i++) {
			//System.out.println("vor: "+stack);
			if(seq[i]>maxSeen) {
				for(int k=maxSeen+1;k<=seq[i];k++) {
					//System.out.println("push. ");
					System.out.print("+");
					stack.push(k);
				}
				maxSeen=seq[i];
			}
			//System.out.println("nach:"+stack);
			//System.out.println("pop. ");
			System.out.print("-");
			if(stack.pop()!=seq[i]) {
				return false;
			} 
		}
		return true;
	}
}
