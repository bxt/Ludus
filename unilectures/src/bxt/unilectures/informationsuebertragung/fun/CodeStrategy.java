package bxt.unilectures.informationsuebertragung.fun;

import java.util.List;
import java.util.Map;

/**
 * From a list of input elements build a binary code 
 */
public interface CodeStrategy {
	/**
	 * From an input sample return a code mapping
	 * @param input A list of inputs to extract e.g. probabilities from
	 * @return A mapping from input elements to binary lists
	 */
	public <T> Map<T,List<Boolean>> getCodeTable(List<T> input);
}
