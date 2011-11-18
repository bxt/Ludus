package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting.ArrayUtils.*;

/**
 * Bubble sort implementation
 * @author Bernhard Häussner
 */
public class BubbleSort implements Sort {
	
	/**
	 * Bubble sort does "bubble" elements up, until it hits
	 * a smaller element, and then bubbles that element up, 
	 * starting over again at the end, until it did not bubble
	 * anything once. 
	 */
	@Override
	public void sort(long[] a) {
		boolean didSwitch = true;
		while (didSwitch) {
			didSwitch  = false;
			for (int j = 1; j < a.length; j++) {
				if(a[j]<a[j-1]) {
					swap(a,j,j-1);
					didSwitch = true;
				}
			}
		}
	}

}
