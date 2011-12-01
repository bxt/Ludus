package bxt.unilectures.informationsuebertragung.fun;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility method to keep track of element counts 
 * @param <T>
 */
public class Counts<T> {
	
	private Map<T,Integer> map;
	
	private Set<T> elements = null;
	
	private Integer sum = null;;
	
	public Counts(Map<T,Integer> map) {
		this.map=Collections.unmodifiableMap(map);
	}
	
	/**
	 * Count the elements into the map
	 * @param input Element list
	 * @return Map with Element->Number of times in list
	 */
	public Counts(List<T> input) {
		Map<T,Integer> counts = new HashMap<T, Integer>();
		for(T t : input) {
			if(counts.get(t)==null)
				counts.put(t, 1);
			else
				counts.put(t, counts.get(t)+1);
		}
		this.map=Collections.unmodifiableMap(counts);
	}
	
	/**
	 * Get the set of elements for which counts exist
	 * @return
	 */
	public Set<T> getElements() {
		if(elements==null) {
			elements=map.keySet();
		}
		return elements;
	}
	
	/**
	 * Get the sum of all element counts
	 * @return
	 */
	public int getSum() {
		if(sum==null) {
			sum=0;
			for(T t : getElements())
				sum+=map.get(t);
		}
		return sum;
	}
	
	/**
	 * Get the count for one element
	 * @param t The element
	 * @return The number of occurrences
	 */
	public Integer getCount(T t) {
		return map.get(t);
	}
	
	/**
	 * Get the probability for one element
	 * @param t The element
	 * @return The number of occurrences
	 */
	public Float getProbability(T t) {
		return ((float)map.get(t)/getSum());
	}
	
}
