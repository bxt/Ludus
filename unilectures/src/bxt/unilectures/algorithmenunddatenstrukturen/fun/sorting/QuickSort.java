package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting.ArrayUtils.*;

/**
 * Quicksort
 * @author Bernhard Häussner
 */
public class QuickSort implements Sort {

	/**
	 * Quicksort sort is divide and conquer sorting by recursively spilltin
	 * in two halves and a pivot, where one part contains the elements
	 * smaller than the pivot and the other part the larger. 
	 */
	@Override
	public void sort(long[] a) {
		quickSort(a, 0, a.length-1);
	}

	/**
	 * The dividing in two halves is done here. 
	 * @param a Complete array to take parts of
	 * @param von Starting index
	 * @param bis Last index (not length)
	 */
	private void quickSort(long[] a, int von, int bis) {
		if(von<bis) {
			int mitte = partition(a,von,bis);
			quickSort(a, von, mitte-1);
			quickSort(a, mitte+1,bis);
		}
	}

	/**
	 * Rearranges the array elements around a pivot
	 * @param a Complete array to take parts of
	 * @param von Starting index
	 * @param bis Last index (not length)
	 * @return bis Pivot position afterwards
	 */
	private int partition(long[] a, int von, int bis) {
		long pivot=a[bis];
		int i=von;
		for(int j=von;j<bis;j++) {
			if(a[j]<pivot) {
				swap(a,i,j);
				i++;
			}
		}
		swap(a,i,bis);
		return i;
	}
		
}
