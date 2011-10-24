package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

public class MergeSort implements Sort {

	@Override
	public void sort(long[] a) {
		mergeSort(a, 0, a.length-1);
	}

	private void mergeSort(long[] a, int von, int bis) {
		if(von<bis) {
			int mitte = (von+bis)/2;
			mergeSort(a, von, mitte);
			mergeSort(a, mitte+1,bis);
			merge(a,von,bis,mitte);
		}
	}
	
	private void merge(long[] a, int von, int bis, int mitte) {
		long[] sorted=new long[bis-von+1];
		int teil1pointer=von;
		int teil2pointer=mitte+1;
		int sortedPointer=0;
		while (teil1pointer <= mitte && teil2pointer <= bis) {
			if(a[teil1pointer] > a[teil2pointer]) {
				sorted[sortedPointer]=a[teil2pointer];
				teil2pointer++;
			} else {
				sorted[sortedPointer]=a[teil1pointer];
				teil1pointer++;
			}
			sortedPointer++;
		}
		while (teil1pointer <= mitte) {
			sorted[sortedPointer]=a[teil1pointer];
			teil1pointer++;
			sortedPointer++;
		}
		while (teil2pointer <= bis) {
			sorted[sortedPointer]=a[teil2pointer];
			teil2pointer++;
			sortedPointer++;
		}
		for (int i=0;i<sorted.length;i++) {
			a[i+von]=sorted[i];
		}
	}
	
}
