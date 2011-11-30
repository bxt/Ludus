package bxt.unilectures.informationsuebertragung.fun;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class CodeStrategySupport {
	/**
	 * Count the elements into a map
	 * @param input Element list
	 * @return Map with Element->Number of times in list
	 */
	protected static <T> Map<T,Integer> count(List<T> input) {
		Map<T,Integer> counts = new HashMap<T, Integer>();
		for(T t : input) {
			if(counts.get(t)==null)
				counts.put(t, 1);
			else
				counts.put(t, counts.get(t)+1);
		}
		return counts;
	}
	
}
