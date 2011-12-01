package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Create a Shannon-Fano code from a list of element occurrences
 */
public class ShannonFano extends CodeStrategySupport implements CodeStrategy {
	
	public <T> Map<T,List<Boolean>> getCodeTable(List<T> input) {
		
		Map<T,Integer> counts = count(input);
		Set<T> countsKeys = counts.keySet();
		
		List<T> list = buildList(counts,countsKeys);
		Map<T,List<Boolean>> codeTable = buildEmptyCodeTable(counts,countsKeys);
		
		populateCodeTable(codeTable,list,counts);
		
		return codeTable;
	}
	
	private static <T> List<T> buildList(
			final Map<T,Integer> counts, Set<T> countsKeys) {
		
		List<T> codepoints = 
				new ArrayList<T>(countsKeys.size());
		for(T t : countsKeys)
			codepoints.add(t);
		
		Collections.sort(codepoints, new Comparator<T>() {
			@Override public int compare(T o1, T o2) {
				return - counts.get(o1).compareTo(counts.get(o2));
			}
		});
		
		return codepoints;
	}
	
	private static <T> Map<T,List<Boolean>> buildEmptyCodeTable(
			Map<T,Integer> counts, Set<T> countsKeys) {
		
		Map<T,List<Boolean>> codeTable = 
				new HashMap<T,List<Boolean>>(countsKeys.size());
		for(T t : countsKeys)
			codeTable.put(t,new ArrayList<Boolean>());
		
		return codeTable;
	}
	
	private static <T> void populateCodeTable(Map<T,List<Boolean>> codeTable, 
			List<T> list, Map<T,Integer> counts) {
		
		if(list.size()<=1) return;
		
		int sum = 0;
		int fullSum = 0;
		for(T t : list)
			fullSum+=counts.get(t);
		
		float bestdiff=5;
		int i=0;
		out: while(i<list.size()) {
			float prediff=bestdiff;
			sum+=counts.get(list.get(i));
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
	
	public static void main(String[] args) {
		List<Character> list=Arrays.asList(new Character[]
				{'a','a','a','a','b','b','b','c','c','d','e','e','e'});
		List<Character> listUq=Arrays.asList(new Character[]
				{'a','b','c','d','e'});
		
		Map<Character,List<Boolean>> table=new ShannonFano().getCodeTable(list);
		
		StringBuffer sb = new StringBuffer();
		sb.append("Code table:\n");
		for(Character c : listUq) {
			sb.append(" "+c+" -> ");
			for(Boolean b : table.get(c)) 
				sb.append(b==Boolean.FALSE?'0':'1');
			sb.append("\n");
		}
		System.out.println(sb.toString());
	}
	
}
