package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import static bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting.ArrayUtils.*;

/**
 * CombinedQuickSort
 * @author Bernhard Häussner
 */
public class CombinedQuickSort implements Sort {
	
	private int threshold = 10;
	
	public CombinedQuickSort() {}

	public CombinedQuickSort(int threshold) {
		super();
		this.threshold = threshold;
	}

	/**
	 * CombinedQuickSort sort a field by splitting it into two part and 
	 * recursively sorting the parts. Once the parts length is beneath a 
	 * threshold, it is sorted with insertion sort. This ways, arrays with 
	 * many equal entries are sorted faster then with usual Quicksort. 
	 */
	@Override
	public void sort(long[] a) {
		quickSort(a, 0, a.length-1);
	}

	/**
	 * Quicksort sort is divide and conquer sorting by recursively splitting
	 * in two halves and a pivot, where one part contains the elements
	 * smaller than the pivot and the other part the larger. 
	 * @param a Complete array to take parts of
	 * @param von Starting index
	 * @param bis Last index (not length)
	 */
	private void quickSort(long[] a, int von, int bis) {
		if(von<bis-threshold) {
			int mitte = partition(a,von,bis);
			quickSort(a, von, mitte-1);
			quickSort(a, mitte+1,bis);
		} else {
			inserionSort(a,von,bis);
		}
	}

	/**
	 * Rearranges the array elements around a pivot, used for Quicksort
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
	
	/**
	 * Do Insertion Sort on a part of an array only
	 * @param a Complete array sort a part of
	 * @param von Starting index
	 * @param bis Last index (not length)
	 */
	public static void inserionSort(long[] a, int von, int bis) {
		for (int j = von+1; j < bis+1; j++) {
			long key = a[j];
			int i = j-1;
			while (i >= von && a[i] > key ) {
				a[i+1] = a[i];
				i--;
			}
			a[i+1] = key;
		}
	}

}
