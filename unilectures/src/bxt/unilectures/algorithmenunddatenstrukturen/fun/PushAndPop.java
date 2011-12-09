package bxt.unilectures.algorithmenunddatenstrukturen.fun;

import java.util.Stack;

public class PushAndPop {
	public static void main(String[] args) {
		int[] seqa={4,3,2,5,1,6,9,8,7,10};
		System.out.println(check(seqa));
		int[] seqb={4,2,1,3,7,6,5,10,9,8};
		System.out.println(check(seqb));
	}
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
