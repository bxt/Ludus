package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

public class InsertionSort implements Sort {

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
