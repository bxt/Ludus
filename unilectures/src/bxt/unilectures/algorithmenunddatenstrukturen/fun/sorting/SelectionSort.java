package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting.ArrayUtils.*;

/**
 * Selection sort implementation
 * @author Bernhard Häussner et al.
 */
public class SelectionSort implements Sort {

	/**
	 * Selection sort picks the smallest element
	 * and swaps it in front of the list until
	 * everything is sorted. 
	 */
	@Override
	public void sort(long[] a) {
		for (int i=0;i<a.length-1;i++) {
			int min = i;
			for (int k=i+1;k<a.length;k++) {
				if(a[k]<a[min]) {
					min=k;
				}
			}
			swap(a,min,i);
		}
	}
	
}
