package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

/**
 * Merge Sort
 * @author Bernhard Häussner
 */
public class MergeSort implements Sort {

	/**
	 * Merge sort is divide and conquer sorting by sorting two halves of 
	 * the unsorted list and then picking the smaller element from the 
	 * two. <p>Unfortunately it is usually not working in-place, so it is
	 * slightly awkwardly adapted here. 
	 */
	@Override
	public void sort(long[] a) {
		mergeSort(a, 0, a.length-1);
	}

	/**
	 * The dividing in two halves is done here. 
	 * @param a Complete array to take parts of
	 * @param von Starting index
	 * @param bis Last index (not length)
	 */
	private void mergeSort(long[] a, int von, int bis) {
		if(von<bis) {
			int mitte = (von+bis)/2;
			mergeSort(a, von, mitte);
			mergeSort(a, mitte+1,bis);
			merge(a,von,bis,mitte);
		}
	}
	
	/**
	 * The conquering by picking the smallest elements and
	 * putting them together is done here. 
	 * @param a Array working on
	 * @param von Starting index
	 * @param bis Last index (not length)
	 * @param mitte Last index from first half
	 */
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
