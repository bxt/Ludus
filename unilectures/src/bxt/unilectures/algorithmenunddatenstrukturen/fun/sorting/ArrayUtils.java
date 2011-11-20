package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

/**
 * Utility methods for handling a long[]
 * @author Bernhard H.
 */
public abstract class ArrayUtils {
	/**
	 * Swap two positions in an array. 
	 * @param a The array to operate on in-place
	 * @param i Switch position 1
	 * @param k Switch position 2
	 */
	public static void swap(final long[] a,final int i,final int k) {
		if(i==k) return;
		a[i]=a[i]^a[k];
		a[k]=a[i]^a[k];
		a[i]=a[i]^a[k];
	}
}
