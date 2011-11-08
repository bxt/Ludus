package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

/**
 * Interface for an in-place sorting algorithm. 
 * @author Bernhard Häussner
 */
public interface Sort {
	/**
	 * The method that does the actual sorting
	 * @param a The array to work on in-place
	 */
	public void sort(long[] a);
}
