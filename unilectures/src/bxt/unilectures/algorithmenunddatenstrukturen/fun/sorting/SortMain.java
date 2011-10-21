package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import java.util.Arrays;

public class SortMain {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		//Sort sort = new InsertionSort();
		Sort sort = new BubbleSort();
		long[] a = {4,6,3,5,8,9,7,2,1};
		System.out.println(Arrays.toString(a));
		sort.sort(a);
		System.out.println(Arrays.toString(a));
		
	}

}
