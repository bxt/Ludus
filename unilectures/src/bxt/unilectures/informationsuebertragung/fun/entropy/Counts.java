package bxt.unilectures.informationsuebertragung.fun.entropy;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Utility method to keep track of element counts 
 * @param <T>
 */
public class Counts<T> {
	
	/**
	 * Units for information
	 * <p>You'd usually use bits. 
	 */
	public static enum Unit {
		BITS(2),BANS(10),NATS(Math.E);
		/**
		 * The base of the logarithm computed for ln-conversion
		 */
		private double logdivisor;
		/**
		 * Specify a unit
		 * @param logbase Base of the logarithm
		 */
		private Unit(double logbase) {
			logdivisor = Math.log(logbase);
		}
		/**
		 * Get the information given a probability
		 * @param probability A probability [0;1]
		 * @return Information in the unit of choice
		 */
		public double calculate(double probability) {
			return - Math.log(probability)/logdivisor;
		}
		public static double convert(double input, Unit from, Unit to) {
			return input*(from.logdivisor/to.logdivisor);
		}
	}
	
	/**
	 * Holds the element counts
	 */
	private Map<T,Integer> map;
	
	/**
	 * Holds all the different elements
	 */
	private Set<T> elements = null;
	
	/**
	 * Holds the sum of the element counts
	 */
	private Integer sum = null;;
	
	/**
	 * Construct by using a counts map
	 * @param map Counts map
	 */
	public Counts(Map<T,Integer> map) {
		this.map=Collections.unmodifiableMap(map);
	}
	
	/**
	 * Count a list of input elements into the map
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
	 * Create a count map from a map to probabilities
	 * <p>Useful if you don't have input samples 
	 * @param Probabilities map
	 * @return Count object filled with equivalent counts 
	 */
	public static <T> Counts<T> fromProbabilities(Map<T, Float> probabilities) {
		Map<T,Integer> counts = new HashMap<T, Integer>();
		Set<T> countSet =  probabilities.keySet();
		Float maxValue=(float)Integer.MAX_VALUE/countSet.size();
		for(T t : countSet) {
			Float count=probabilities.get(t)*maxValue;
			if(! (count<maxValue) ) { // Can't change to c>m b/c NaN
				count=maxValue;
			}
			counts.put(t, Math.round(count));
		}
		return new Counts<T>(counts);
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
		Integer returnValue = map.get(t);
		if(returnValue==null) returnValue=0;
		return returnValue;
	}
	
	/**
	 * Get the probability for one element
	 * @param t The element
	 * @return The number of occurrences
	 */
	public float getProbability(T t) { // p(t)
		return ((float)getCount(t)/getSum());
	}
	
	/**
	 * Get the surprise factor of one certain element according to the 
	 * probabilities derived from the counts
	 * @param t The element
	 * @param unit The unit to calculate result in
	 * @return The self information in [unit]
	 */
	public double getOneSelfInformation(T t, Unit unit) { // I
		return unit.calculate(getProbability(t));
	}
	
	/**
	 * Get the surprise factor if all probabilities were equal for the given 
	 * elements
	 * @param unit The unit to calculate result in
	 * @return The self maximum possible information in [unit]
	 */
	public double getMaximumSelfInformation(Unit unit) { // H0
		return - unit.calculate(getElements().size());
	}
	
	/**
	 * Get the expected surprise factor according to counted probabilities
	 * when reading a stream
	 * @param unit The unit to calculate result in
	 * @return The mean information per read element in [unit]
	 */
	public double getSelfInformation(Unit unit) { // H* = E[I]
		double returnValue=0;
		for(T t : getElements()) {
			Double info=getOneSelfInformation(t,unit);
			if(info.isInfinite()) returnValue+=0; // p(t)=0
			else returnValue+=getProbability(t)*info;
		}
		return returnValue;
	}
	
	/**
	 * For a code table and our probabilities calculate the expected 
	 * length of one encoded element
	 * @param codeTable Mapping to take the lengths from
	 * @return The mean number of output symbols for one input symbol
	 */
	public <V> double getMeanLength(Map<T,List<V>> codeTable) { // Sm
		double returnValue=0;
		for(T t : getElements()) {
			List<V> vector = codeTable.get(t);
			returnValue+=getProbability(t)*(vector==null?0:vector.size());
		}
		return returnValue;
	}
	
	/**
	 * Return the potential savings when using an optimal encoding
	 * @param codeTable The non optimal encoding
	 * @param unit The unit to calculate result in
	 * @return
	 */
	public <V> double getRedundancy(Map<T, List<V>> codeTable, Unit unit) {
		return getMeanLength(codeTable)-getSelfInformation(unit);
	}
	
	/**
	 * Get the compression percentage of the number of bits needed for
	 * a minimal block code
	 * @param codeTable  The target encoding
	 * @param unit The unit to base the block encoding on
	 * @return A number x of [0;1] with block*x=target 
	 */
	public <V> double getCompressionRatio(Map<T, List<V>> codeTable, Unit unit){
		return getMeanLength(codeTable)/getCeiledDesicionContent(unit);
	}
	
	/**
	 * Number of bits needed for a minimal block code
	 * @param unit The unit to base the block encoding on
	 * @return
	 */
	public <V> double getCeiledDesicionContent(Unit unit) {
		return Math.ceil(getMaximumSelfInformation(unit));
	}
	
	/**
	 * Output some facts about a list of probabilities
	 * @param args
	 */
	public static void main(String... args) {
		
		Map<Character,Float> probabilities=new LinkedHashMap<Character,Float>();
		probabilities.put('a', 0.5F);
		probabilities.put('b', 0.5F);
		probabilities.put('c', 0.125F);
		probabilities.put('d', 0.125F);
		probabilities.put('e', 0F);
		Counts<Character> counts = Counts.fromProbabilities(probabilities);
		
		System.out.println("Probability of a");
		System.out.println(counts.getProbability('a'));
		System.out.println("Self Information of a (bits)");
		System.out.println(counts.getOneSelfInformation('a',Unit.BITS));
		System.out.println("Probability of e");
		System.out.println(counts.getProbability('e'));
		System.out.println("Self Information of e (bits)");
		System.out.println(counts.getOneSelfInformation('e',Unit.BITS));
		
		System.out.println("Maximum Self Information (bits)");
		System.out.println(counts.getMaximumSelfInformation(Unit.BITS));
		System.out.println("Actual Self Information (bits)");
		System.out.println(counts.getSelfInformation(Unit.BITS));
		
		double convert=4.2;
		System.out.printf("PS, %.3f nats is about %.3f bits and %.3f bans!\n",
				convert,
				Unit.convert(convert, Unit.NATS, Unit.BITS),
				Unit.convert(convert, Unit.NATS, Unit.BANS));
	}

}
