package bxt.unilectures.informationsuebertragung.fun.redundancy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import bxt.unilectures.informationsuebertragung.fun.entropy.CodeStrategy;
import bxt.unilectures.informationsuebertragung.fun.entropy.Counts;

/**
 * Create a Block code from a list of element occurrences
 */
public class Block implements CodeStrategy {
	
	/**
	 * This method yields it all together
	 * @param input A list of inputs to extract probabilities from
	 * @return A mapping from input elements to binary lists
	 */
	public <T> Map<T,List<Boolean>> getCodeTable(Counts<T> counts) {
		
		Set<T> countsKeys = counts.getElements();
		
		Map<T,List<Boolean>> codeTable = 
				new HashMap<T,List<Boolean>>(countsKeys.size());
		
		int i=0;
		for(T t: countsKeys) {
			codeTable.put(t, buildBlockChiffre(i,countsKeys.size()));
			i++;
		}
		
		return codeTable;
	}
	
	/**
	 * Convert a index into its fixed-length binary representation
	 * @param i A positive number
	 * @param of Maximum number (defines number of leading zeros)
	 * @return
	 */
	private static List<Boolean> buildBlockChiffre(int i,int of) {
		List<Boolean> chiffre=new ArrayList<Boolean>();
		int k=1;
		while(k<=of)
			k=k*2;
		while(k>1) {
			k=k/2;
			if(i>=k) {
				chiffre.add(Boolean.TRUE);
				i-=k;
			} else {
				chiffre.add(Boolean.FALSE);
			}
		}
		return chiffre;
	}
}
