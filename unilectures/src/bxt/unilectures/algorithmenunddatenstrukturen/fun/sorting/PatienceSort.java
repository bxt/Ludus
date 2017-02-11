package bxt.unilectures.algorithmenunddatenstrukturen.fun.sorting;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

/**
 * Patience Sort
 * @author Bernhard Häussner
 */
public class PatienceSort implements Sort {

	/**
	 * Compares two {@link Deque}s by their first element, with empties last.
	 */
	private static final Comparator<Deque<Long>> PILE_COMPARATOR = Comparator.comparing(Deque::peek, Comparator.nullsLast(Comparator.naturalOrder()));

	/**
	 * Patience Sort works similar to the game Patience, or solitaire. It first
	 * puts all elements onto sorted piles (stacks) and then builds the result
	 * from those piles using a k-way-merge.
	 */
	@Override
	public void sort(long[] a) {
		List<Deque<Long>> piles = buildPiles(a);
		kWayMerge(piles, a);	
	}
	
	/**
	 * This builds the piles using the following rules:
	 *  <li> For the first element build a pile with this element only
	 *  <li> For each element <tt>x</tt> put it on the first pile with <tt>tip &gt;= x</tt>
	 *  <li> If there is no such pile, put it on a new pile at the end
	 * 
	 * @param elements The input array.
	 * @return
	 * 		The piles containing all elements from <tt>a</tt>. The individual
	 * 		piles are sorted and the number of piles is the lenght of a longest
	 * 		increasing sequence in <tt>a</tt>.
	 * 
	 */
	private List<Deque<Long>> buildPiles(long[] elements) {
		List<Deque<Long>> piles = new ArrayList<>();
		for (long x : elements) {
			Deque<Long> pile = new LinkedList<>();
			pile.push(x);
			int index = Collections.binarySearch(piles, pile, PILE_COMPARATOR);
			if (index < 0) {
				index = -index - 1;
			}
			if (index >= piles.size()) {
				piles.add(pile);
			} else {
				piles.get(index).push(x);
			}
		}
		return piles;
	}
	
	/**
	 * This merges <tt>k</tt> stacks (the piles) of sorted elements into one
	 * single sorted array. It does this by placing the stacks in a priority
	 * queue indexed by the topmost stack element. It then repeatedly pops the
	 * top element from the stack with the lowest element and updates the
	 * priority queue since this stack will then have a new topmost value. It
	 * is done when all stacks are empty.
	 * 
	 * @param piles Sorted stacks of elements.
	 * @param into Will be filled with the elements of the stacks in sorted order.
	 */
	private void kWayMerge(List<Deque<Long>> piles, long[] into) {
		if (piles.isEmpty()) return;
		
		Queue<Deque<Long>> priorityQueue = new PriorityQueue<>(PILE_COMPARATOR);
		priorityQueue.addAll(piles);
		
		for (int i = 0; !priorityQueue.peek().isEmpty(); i++) {
			Deque<Long> pile = priorityQueue.remove();
			into[i] = pile.pop();
			priorityQueue.add(pile); // actually should Re-heapify, but this is not possible with this implementation
		}
	}
}
