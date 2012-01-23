package bxt.unilectures.informationsuebertragung.fun.entropy;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Turn fixed set of elements into a binary prefix code using lookup table
 * @author Burny
 * @param <T> Type of elements to encode
 */
public class Code<T> {
	
	/**
	 * Save the lookup dictionary
	 */
	private Map<T,List<Boolean>> map = null;
	/**
	 * Caches the fsm for decoding once initialized
	 */
	private StateMachine<State<T>,Boolean> machine = null;
	
	/**
	 * Construct a Code with a certain dictionary map
	 * <p>Ensure the mapping's extension in injective yourself. 
	 * @param map lookup table mapping element to binary list
	 */
	public Code(Map<T,List<Boolean>> map) {
		this.map=map;
	}
	
	/**
	 * Encode a list of elements into a binary string
	 * @param plain A sequence of elements
	 * @return A sequence of true/false
	 */
	public List<Boolean> encode(List<T> plain) {
		List<Boolean> chiffre = new LinkedList<Boolean>();
		for(T t : plain)
			chiffre.addAll(map.get(t));
		return chiffre;
	}
	
	/**
	 * Undo encoding, decode a binary string to a list of elements
	 * <p>Will fail badly on invalid inputs
	 * @param chiffre A sqeuence of true/false
	 * @return A sequence of elements
	 */
	public List<T> decode(List<Boolean> chiffre) {
		if(machine==null) buildStateMachine();
		List<T> plain = new LinkedList<T>();
		for(Boolean b : chiffre) {
			State<T> state = machine.step(b);
			if(state.result != null) { 
				plain.add(state.result);
				machine.reset();
			}
		}
		return plain;
	}
	
	/**
	 * Internal method that figures transitions and states of the decoder
	 */
	private void buildStateMachine() {
		State<T> startState=new State<T>(null);
		
		// Cache of upstream states
		Map<String,State<T>> stateMap = new HashMap<String, State<T>>();
		
		machine = new StateMachine<State<T>, Boolean>(startState);
		
		for (T t : map.keySet()) {
			State<T> prevState=startState;
			List<Boolean> chiffre = map.get(t);
			StringBuilder stringBuilder = new StringBuilder();
			// Add states and transitions for this chiffre:
			for(int i=0;i<chiffre.size();i++) {
				// Keep track of used states using strings:
				stringBuilder.append(chiffre.get(i).toString());
				State<T> state=stateMap.get(stringBuilder.toString());
				if(state == null) {
					if(i==chiffre.size()-1) {
						// The final state will point us back to our element
						state = new State<T>(t);
					} else {
						state = new State<T>(null);
					}
				}
				stateMap.put(stringBuilder.toString(), state);
				machine.addTransition(prevState, chiffre.get(i), state);
				prevState=state;
			}
		}
	}
	
	/**
	 * Internal State class for use in the fsm
	 * @param <T> Type of elements beeing decoded
	 */
	private static class State<T> {
		private T result;
		private State(T result) {
			this.result = result;
		}
	}

	/**
	 * Main method to test our en-/decoding
	 * <p>
	 * Will first create the code [0->val1, 10->val2, 110->val3] and then
	 * print [val1,val2] encoded and [10100] decoded. 
	 * @param args
	 */
	public static void main(String[] args) {
				
		Map<String,List<Boolean>> codeMap = new HashMap<String,List<Boolean>>();
		
		{	List<Boolean> list=new ArrayList<Boolean>(3);
			list.add(Boolean.FALSE);
			codeMap.put("val1",list);    }
		{	List<Boolean> list=new ArrayList<Boolean>(3);
			list.add(Boolean.TRUE);list.add(Boolean.FALSE);
			codeMap.put("val2",list);    }
		{	List<Boolean> list=new ArrayList<Boolean>(3);
			list.add(Boolean.TRUE);list.add(Boolean.TRUE);list.add(Boolean.FALSE);
			codeMap.put("val3",list);    }
		
		Code<String> code = new Code<String>(codeMap);
		
		List<String> listInp=new ArrayList<String>(3);
		listInp.add("val1");listInp.add("val2");
		System.out.println(code.encode(listInp));
		
		List<Boolean> list=new ArrayList<Boolean>(5);
		list.add(Boolean.TRUE);list.add(Boolean.FALSE);list.add(Boolean.TRUE);
		list.add(Boolean.FALSE);list.add(Boolean.FALSE);
		
		System.out.println(code.decode(list));
		
	}

}
