package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Create a Shannon-Fano code from a list of element occurrences
 */
public class ShannonFano implements CodeStrategy {
	
	/**
	 * Call methods to build the list, create an empty code table, and
	 * then start recursively populating the code table
	 */
	public <T> Map<T,List<Boolean>> getCodeTable(Counts<T> counts) {
		
		List<T> list = buildList(counts);
		Map<T,List<Boolean>> codeTable = buildEmptyCodeTable(counts);
		
		populateCodeTable(codeTable,list,counts);
		
		return codeTable;
	}
	
	/**
	 * Sort our input probability list descending
	 * @param counts Element counts
	 * @return Elements sorted by probability
	 */
	private static <T> List<T> buildList(final Counts<T> counts) {
		
		Set<T> countsKeys = counts.getElements();
		
		List<T> list = 
				new ArrayList<T>(countsKeys.size());
		for(T t : countsKeys)
			list.add(t);
		
		Collections.sort(list, new Comparator<T>() {
			@Override public int compare(T o1, T o2) {
				return - counts.getCount(o1).compareTo(counts.getCount(o2));
			}
		});
		
		return list;
	}
	
	/**
	 * Build an empty code table, i.e. zero length bit vector for
	 * each element 
	 * @param counts Element counts
	 * @return The empty bit vector code table
	 */
	private static <T> Map<T,List<Boolean>> buildEmptyCodeTable(
			Counts<T> counts) {
		
		Set<T> countsKeys = counts.getElements();
		
		Map<T,List<Boolean>> codeTable = 
				new HashMap<T,List<Boolean>>(countsKeys.size());
		for(T t : countsKeys)
			codeTable.put(t,new ArrayList<Boolean>());
		
		return codeTable;
	}
	
	/**
	 * Recursively populate the code table. 
	 * <p>Split in two parts with (mostly) equal probabilities. For one part 
	 * add zeros for the other ones to the bit vectors. Then split those parts
	 * again until they contain only one element. 
	 * @param codeTable The code table filled so far (in place)
	 * @param list Part of the list to process
	 * @param counts Element counts
	 */
	private static <T> void populateCodeTable(Map<T,List<Boolean>> codeTable, 
			List<T> list, Counts<T> counts) {
		
		if(list.size()<=1) return;
		
		int sum = 0;
		int fullSum = 0;
		for(T t : list)
			fullSum+=counts.getCount(t);
		
		float bestdiff=5;
		int i=0;
		out: while(i<list.size()) {
			float prediff=bestdiff;
			sum+=counts.getCount(list.get(i));
			bestdiff = Math.abs((float)sum/fullSum-0.5F);
			if(prediff<bestdiff) break out;
			i++;
		}
		
		for(int j=0;j<list.size();j++) {
			if(j<i)
				codeTable.get(list.get(j)).add(Boolean.FALSE);
			else
				codeTable.get(list.get(j)).add(Boolean.TRUE);
		}
		
		populateCodeTable(codeTable, list.subList(0, i), counts);
		populateCodeTable(codeTable, list.subList(i, list.size()), counts);
	}
	
	/**
	 * Tests building a code table
	 * @param args
	 */
	public static void main(String[] args) {
		Map<Character,Integer> countsMap=new LinkedHashMap<Character, Integer>();
		countsMap.put('a', 4);
		countsMap.put('b', 3);
		countsMap.put('c', 2);
		countsMap.put('d', 1);
		countsMap.put('e', 3);
		Counts<Character> counts=new Counts<Character>(countsMap);
		
		Map<Character,List<Boolean>> table=
				new ShannonFano().getCodeTable(counts);
		
		StringBuffer sb = new StringBuffer();
		sb.append("Code table:\n");
		for(Character c : countsMap.keySet()) {
			sb.append(" "+c+" -> ");
			for(Boolean b : table.get(c)) 
				sb.append(b==Boolean.FALSE?'0':'1');
			sb.append("\n");
		}
		System.out.println(sb.toString());
	}
	
}
