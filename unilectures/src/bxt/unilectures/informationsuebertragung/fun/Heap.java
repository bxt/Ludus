package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.List;

/**
 * A generic max/min-heap priority Queue. 
 * @author Bernhard Häussner
 */
public class Heap<T extends Comparable<T>> {
	
	/**
	 * Index of top heap element
	 */
	private static int TOP = 0;
	
	/**
	 * To select min or max heap
	 */
	public static enum Ordering {
		/**
		 * Maxheap, i.e. big elements on top
		 */
		MAX(1),
		/**
		 * Minheap, i.e. small elements on top
		 */
		MIN(-1);
		
		/**
		 * Will be matched against the return value of 
		 * {@link Comparable#compareTo(Object)}
		 */
		private final int compareSign;
		
		private Ordering(int cS) {
			compareSign=cS;
		}
	}
	
	/**
	 * Use maxheap if no ordering is selected
	 */
	private static Ordering defaultOrdering = Ordering.MAX;
	
	/**
	 * The actual heap data
	 */
	private final List<T> a=new ArrayList<T>();
	/**
	 * Is this heap a min or max heap
	 */
	private final Ordering ordering;
	
	/**
	 * @see Heap#Heap(Comparable[], Ordering)
	 * @param as
	 */
	public Heap(final T[] as) {
		this(as,defaultOrdering);
	}
	
	/**
	 * @see Heap#Heap(Collection, Ordering)
	 * @param as
	 */
	public Heap(final Collection<T> as) {
		this(as,defaultOrdering);
	}
	
	/**
	 * Construct using an array
	 * @param a Array to build heap from
	 * @return A fresh Instance working an a copied max-heaped array
	 */
	public Heap(final T[] as, Ordering ordering) {
		this.ordering=ordering;
		Collections.addAll(a, as);
		for(int i=a.size()-1;i>=0;i--) {
			reHeapify(i);
		}
	}
	
	/**
	 * Construct using another Collection
	 * @param a Array to build heap from
	 * @return A fresh Instance working an a copied max-heaped array
	 */
	public Heap(final Collection<T> as, Ordering ordering) {
		this.ordering=ordering;
		a.addAll(as);
		for(int i=a.size()-1;i>=0;i--) {
			reHeapify(i);
		}
	}
	
	/**
	 * Get and remove topmost element, preserve heap property. 
	 * @return The former greatest element
	 */
	public T pop() {
		if(a.size()<1) throw new EmptyStackException();
		T maxvalue=a.get(TOP);
		a.set(TOP, a.get(a.size()-1));
		a.remove(a.size()-1);
		if(a.size()>0) reHeapify(TOP);
		return maxvalue;
	}		
	
	/**
	 * Add another element, preserve heap property. 
	 * @param newElement Element that will be on the heap afterwards
	 */
	public void push(T newElement) {
		int i=a.size();
		a.add(newElement);
		while(i>0 && a.get(i).compareTo(a.get((i-1)/2))==ordering.compareSign) {
			T tmp=a.get(i);
			a.set(i, a.get((i-1)/2));
			a.set((i-1)/2, tmp);
			i=(i-1)/2;
		}
	}
	
	/**
	 * Get the number of elements in currently the heap
	 * @return element count
	 */
	public int size() {
		return a.size();
	}
	
	/**
	 * Fix a part of the heap bottom up from a too small element downwards. 
	 * @param i Offending parent element index
	 */
	private void reHeapify(final int i) {
		final int l=i*2+1, r=i*2+2, s=a.size();
		int largest;
		if(l<s && a.get(l).compareTo(a.get(i))==ordering.compareSign ) 
			largest=l;
		else largest=i;
		if(r<s && a.get(r).compareTo(a.get(largest))==ordering.compareSign )
			largest=r;
		if(largest!=i) {
			T tmp=a.get(i);
			a.set(i, a.get(largest));
			a.set(largest, tmp);
			reHeapify(largest);
		}
	}
	
	/**
	 * Make a neat ASCII tree from the heap. 
	 */
	public String toString() {
		final int s=a.size();
		final StringBuffer stringBuffer = new StringBuffer();
		
		int breakIndex=0,breakStep=1,heapWidth=1;
		while (heapWidth*2<=s) heapWidth=heapWidth*2;
		int elemWidth=heapWidth;
		
		for(int i=0;i<s;i++) {
			stringBuffer.append(strRepeat(" ",elemWidth*2));
			stringBuffer.append(" "+a.get(i).toString());
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
	
	/**
	 * A method to test the heap. Will arrange the numbers from 1-9 into a 
	 * maxheap and print it. 
	 * @param args
	 */
	public static void main(String[] args) {
		Heap<Integer> heap = new Heap<Integer>(new Integer[]{1,2,3,4,5,6,7,8,9}, Ordering.MAX);
		System.out.println(heap);
	}

}