package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import bxt.unilectures.informationsuebertragung.fun.Heap.Ordering;

/**
 * Create a Huffman code from a list of element occurrences
 */
public class Huffman extends CodeStrategySupport implements CodeStrategy {
	
	/**
	 * This method yields it all together
	 * @param input A list of inputs to extract probabilities from
	 * @return A mapping from input elements to binary lists
	 */
	public <T> Map<T,List<Boolean>> getCodeTable(List<T> input) {
		
		Map<T,Integer> counts = count(input);
		Set<T> countsKeys = counts.keySet();
		Heap<Codepoint<T>> heap = buildCodepointHeap(counts,countsKeys);
		rearrangeHeap(heap);
		
		Map<T,List<Boolean>> codeTable = 
				new HashMap<T,List<Boolean>>(countsKeys.size());
		populateCodeTable(codeTable,heap.pop(),new ArrayList<Boolean>());
		
		return codeTable;
	}
	
	/**
	 * Build the priority queue from the elements sorted by their
	 * occurrence probability
	 * @param counts Map of element counts
	 * @param countsKeys Set of all occouring elements
	 * @return Heap with the elementes wrapped in {@link Codepoint}s
	 */
	private static <T> Heap<Codepoint<T>> buildCodepointHeap(
			Map<T,Integer> counts, Set<T> countsKeys) {
		Collection<Codepoint<T>> codepoints = 
				new ArrayList<Codepoint<T>>(countsKeys.size());
		for(T t : countsKeys)
			codepoints.add(new Codepoint<T>(t,counts.get(t)));
		
		Heap<Codepoint<T>> heap = 
				new Heap<Codepoint<T>>(codepoints,Ordering.MIN);
		//System.out.println(heap);
		
		return heap;
	}
	
	/**
	 * Repeatedly takes the smallest two elements from the heap and 
	 * heaps them together into new tiny heaps 
	 * <p>However, the tiny heaps are just object refernces, not the
	 * pro queue stuff. 
	 * @param heap The heap to operate on
	 */
	private static <T> void rearrangeHeap(Heap<Codepoint<T>> heap) {
		while(heap.size()>1) {
			heap.push(Codepoint.merge( heap.pop(),heap.pop()));
			//System.out.println(heap);
		}
	}
	
	/**
	 * From the tiny heaps recursively build binary codes for each element
	 * @param codeTable Code table populated so far (in-place)
	 * @param codepoint Current codepoint (recursively split into children)
	 * @param chiffre Current binary code (adds digits with recursion depth)
	 */
	private static <T> void populateCodeTable(Map<T,List<Boolean>> codeTable, 
			Codepoint<T> codepoint, List<Boolean> chiffre) {
		if(codepoint.target != null) {
			//System.out.println(codepoint.target.toString()+chiffre.toString());
			codeTable.put(codepoint.target, chiffre);
		} else {
			List<Boolean> chiffreLeft=new ArrayList<Boolean>(chiffre);
			chiffreLeft.add(Boolean.FALSE);
			populateCodeTable(codeTable,codepoint.childLeft,chiffreLeft);
			List<Boolean> chiffreRight=new ArrayList<Boolean>(chiffre);
			chiffreRight.add(Boolean.TRUE);
			populateCodeTable(codeTable,codepoint.childRight,chiffreRight);
		}
	}
	
	/**
	 * Helper Class for sorting the codepoints on the heap and 
	 * contains the tiny heaps to build the binary code too 
	 * @param <T> Element type
	 */
	private static class Codepoint<T> implements Comparable<Codepoint<T>> {
		/**
		 * If this is a leaf, contains the encoded element
		 */
		private T target;
		/**
		 * Contains the weight of this node (sum of child probabilities)
		 */
		private int count;
		/**
		 * If this is a non-leaf node contains the first (0-encoded) child
		 */
		private Codepoint<T> childLeft = null;
		/**
		 * If this is a non-leaf node contains the second (1-encoded) child
		 */
		private Codepoint<T> childRight = null;
		/**
		 * Quickly construct a codepoint
		 * @param target
		 * @param count
		 */
		public Codepoint(T target, int count) {
			this.target = target;
			this.count = count;
		}
		/**
		 * For the sorting, compares probabilities
		 */
		@Override public int compareTo(Codepoint<T> other) {
			int cmp=Integer.valueOf(count).compareTo(other.count);
			return cmp;
		}
		/**
		 * Useful for debugging the code
		 */
		@Override public String toString() {
			if(target!=null)
				return target.toString()+"#"+count;
			return "("+childLeft.toString()+"|"+childRight.toString()+")#"+count;
		}
		/**
		 * Create a new codepoint with two child codepoints (build tiny heap)
		 * @param c1 first (0-encoded) child
		 * @param c2 second (1-encoded) child
		 * @return
		 */
		public static <T> Codepoint<T> merge(Codepoint<T> c1, Codepoint<T> c2) {
			Codepoint<T> newCodepoint = new Codepoint<T>(null,c1.count+c2.count);
			newCodepoint.childLeft = c1;
			newCodepoint.childRight = c2;
			return newCodepoint;
		}
	}
}
