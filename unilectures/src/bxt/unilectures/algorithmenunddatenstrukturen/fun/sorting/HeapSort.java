package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import java.util.Arrays;
import java.util.EmptyStackException;

/**
 * Heap sort Implementation
 * @author Bernhard Häussner
 */
public class HeapSort implements Sort {
	
	/**
	 * Will sort an array by constructing a heap first and 
	 * repeatedly poping the biggest elements afterwards. 
	 */
	@Override
	public void sort(long[] a) {
		final Heap h = new Heap(a);
		for(int i=a.length/2-1;i>=0;i--) {
			h.maxHeapify(i);
		}
		for (int i=0;i<a.length-1;i++) {
			h.popMaxvalue();
		}
	}
	
	/**
	 * Will sort an array and print out intermediate heaps. 
	 * @param args
	 */
	public static void main(final String... args) {
		
		// Some stuff to sort:
		final long[] a = {3,4,9,1,2,5,6,2,8,10,7,23,11,6,12};
		final Heap h = new Heap(a);
		
		System.out.println("Initial, borken heap:");
		System.out.println(h);
		for(int i=a.length/2-1;i>=0;i--) {
			h.maxHeapify(i);
			System.out.println(" A little more fixed heap:");
			System.out.println(h);
		}
		System.out.println("Fixed heap:");
		System.out.println(h);
		for (int i=0;i<a.length-1;i++) {
			long max=h.popMaxvalue();
			System.out.println(" Max:"+max+", still unsorted heap:");
			System.out.println(h);
		}
		System.out.println(" Nearly all popped heap:");
		System.out.println(h);
		
		System.out.println("Done sorting:");
		System.out.println(Arrays.toString(a));
	}
	
	/**
	 * A max-heap priority 1ueue working on long primitives. 
	 * @author Bernhard Häussner
	 */
	public static class Heap {
		
		private long[] a;
		private int s;
		
		/**
		 * Private Constructor for modifying elements in-place. 
		 * @param a Array modified in situ
		 */
		private Heap(final long[] a) {
			this.a = a;
			this.s = a.length;
		}
		
		/**
		 * "Public Constructor" to generate OOP-safe instances. 
		 * @param a Array to build heap from
		 * @return A fresh Instance working an a copied max-heaped array
		 */
		public static Heap getInstance(final long[] a) {
			long[] ma_a=new long[a.length];
			System.arraycopy(a, 0, ma_a, 0, a.length);
			Heap heap = new Heap(ma_a);
			for(int i=a.length-1;i>=0;i--) {
				heap.maxHeapify(i);
			}
			return heap;
		}
		
		/**
		 * Get and remove topmost element, preserve heap ordering. 
		 * @return The former greatest element
		 */
		public long popMaxvalue() {
			if(s<1) throw new EmptyStackException();
			s--;
			swap(a,0,s);
			if(s>0) maxHeapify(0);
			return a[s];
		}
		
		/**
		 * Fix a part of the heap bottom up from a too small element downwards. 
		 * @param i Offending parent element index
		 */
		private void maxHeapify(final int i) {
			final int l=left(i), r=right(i);
			int largest;
			if(l<s && a[l] > a[i]) largest=l;
			else largest=i;
			if(r<s && a[r] > a[largest]) largest=r;
			if(largest!=i) {
				swap(a,i,largest);
				maxHeapify(largest);
			}
		}
		
		/**
		 * Swap two positions in an array. 
		 * @param a The array to operate on in-place
		 * @param i Switch position 1
		 * @param k Switch position 2
		 */
		private static void swap(final long[] a,final int i,final int k) {
			if(i==k) return;
			a[i]=a[i]^a[k];
			a[k]=a[i]^a[k];
			a[i]=a[i]^a[k];
		}
		
		/**
		 * Get left child of an element. 
		 * @param i Index of the parent element
		 * @return Index of the left child element
		 */
		private static int left(final int i) {
			return i*2+1;
		}

		/**
		 * Get right child of an element. 
		 * @param i Index of the parent element
		 * @return Index of the right child element
		 */
		private static int right(final int i) {
			return i*2+2;
		}
		
		/**
		 * Make a neat ASCII tree from the heap. 
		 */
		public String toString() {
			final StringBuffer stringBuffer = new StringBuffer();
			
			int breakIndex=0,breakStep=1,heapWidth=1;
			while (heapWidth*2<=s) heapWidth=heapWidth*2;
			int elemWidth=heapWidth;
			
			for(int i=0;i<s;i++) {
				stringBuffer.append(strRepeat(" ",elemWidth*2));
				stringBuffer.append(a[i]<10?" "+a[i]:a[i]);
				stringBuffer.append(strRepeat(" ",(elemWidth-1)*2));
				if(i==breakIndex) {
					stringBuffer.append("\n");
					//stringBuffer.append("\n");
					breakStep=2*breakStep;
					breakIndex+=breakStep;
					elemWidth=elemWidth/2;
				}
			}
			
			stringBuffer.append("\n");
			return stringBuffer.toString();
		}
		
		/**
		 * Concatenate n string copys. 
		 * @param string String to multiply
		 * @param times Number of times duplicated
		 * @return The repeated strings
		 */
		private static String strRepeat(final String string,final int times) {
			final StringBuffer stringBuffer = new StringBuffer();
			for (int i=0;i<times;i++)
				stringBuffer.append(string);
			return stringBuffer.toString();
		}
	}
	
}