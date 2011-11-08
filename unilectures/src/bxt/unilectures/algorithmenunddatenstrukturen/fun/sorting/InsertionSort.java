package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

/**
 * Insertion sort implementation
 * @author Bernhard Häussner
 */
public class InsertionSort implements Sort {

	/**
	 * Insertions sort incrementally builds a sorted list by
	 * putting the next element in the right place, shifting the
	 * greater onces left away. 
	 */
	@Override
	public void sort(long[] a) {
		for (int j = 1; j < a.length; j++) {
			long key = a[j];
			int i = j-1;
			while (i >= 0 && a[i] > key ) {
				a[i+1] = a[i];
				i--;
			}
			a[i+1] = key;
		}
	}

}
