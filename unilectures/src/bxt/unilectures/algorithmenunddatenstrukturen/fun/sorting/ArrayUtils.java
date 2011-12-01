package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import java.util.Arrays;

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
	
	// (PrÜ1)
	
	/**
	 * Shift all array elements 1 position to the left, append
	 * first 1 elements at the end. 
	 * @param a The array to operate on in-place
	 */
	public static void rotateLeft(final long[] a) {
		long tmp=a[0];
		int n=a.length;
		for(int i=0;i<n-1;i++) {
			a[i]=a[i+1];
		}
		a[n-1]=tmp;
	}
	
	/**
	 * Mirror a range of an array
	 * @param a The array to operate on in-place
	 * @param l Index of first element to mirror
	 * @param r Index of last element to mirror
	 */
	public static void mirrorRange(long[] a, int l, int r) {
		if(0<=l && l<r && r < a.length) {
			swap(a,l,r);
			mirrorRange(a,l+1,r-1);
		}
	}
	
	/**
	 * Count how many pairs of all pairs of elements sum to k
	 * @param a The array to read the elements from
	 * @param k The desired sum
	 * @return The number of pairs
	 */
	public static int countSumPairsEqual(long[] a, int k) {
		int n=a.length;
		int c=0;
		for(int i=0;i<n-1;i++) {
			for(int j=i+1; j<n;j++) {
				if(a[i]+a[j]==k) {
					c++;
				}
			}
		}
		return c;
	}
	
	/**
	 * Shift all array elements n positions to the left, append
	 * first n elements at the end. 
	 * @param a The array to operate on in-place
	 * @param n Number of positions to shift
	 */
	public static void rotateLeft(final long[] a, int n) {
		if(n<0) {
			rotateRight(a,-n);
			return;
		}
		long[] tmp=new long[n];
		System.arraycopy(a, 0, tmp, 0, n);
		for(int i=0;i<a.length-n;i++) {
			a[i]=a[i+n];
		}
		System.arraycopy(tmp, 0, a, a.length-n, n);
	}
	
	/**
	 * Shift all array elements n positions to the right, prepend
	 * last n elements to the start at the end. 
	 * @param a The array to operate on in-place
	 * @param n Number of positions to shift
	 */
	public static void rotateRight(final long[] a, int n) {
		if(n<0) {
			rotateLeft(a,-n);
			return;
		}
		long[] tmp=new long[n];
		System.arraycopy(a, a.length-n, tmp, 0, n);
		for(int i=a.length-1;i>=n;i--) {
			int ip=(i-n+a.length)%a.length;
			a[i]=a[ip];
		}
		System.arraycopy(tmp, 0, a, 0, n);
	}
	
	/**
	 * Demonstrates the methods defined in this class
	 * @param args
	 */
	public static void main(String[] args) {
		long[] a={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
		System.out.println(Arrays.toString(a));
		rotateLeft(a,4);
		System.out.println(Arrays.toString(a));
		rotateLeft(a,-4);
		System.out.println(Arrays.toString(a));
		mirrorRange(a, 3, 8);
		System.out.println(Arrays.toString(a));
		rotateLeft(a);
		rotateLeft(a);
		rotateLeft(a);
		System.out.println(Arrays.toString(a));
		swap(a,3,7);
		System.out.println(Arrays.toString(a));
		System.out.println(countSumPairsEqual(a,7));
	}
}
