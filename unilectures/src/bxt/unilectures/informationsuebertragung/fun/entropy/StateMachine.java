package bxt.unilectures.informationsuebertragung.fun.entropy;

import java.util.HashMap;
import java.util.Map;

/**
 * A State Machine with one state at a time
 * @author Burny
 * @param <S> State type
 * @param <I> Input type
 */
public class StateMachine<S,I> {
	
	/**
	 * For each state save for each input which state comes next
	 */
	private Map<S,Map<I,S>> transitions = new HashMap<S,Map<I,S>>();
	
	/**
	 * Current state the fsm is in
	 */
	private S state = null;
	/**
	 * Start state to go back into when {@link #reset()} is called. 
	 */
	private S startState = null;
	
	/**
	 * Construct the machine
	 * <p>Note that the transitions are added later. 
	 * @param startState
	 */
	public StateMachine(S startState) {
		this.state=startState;
		this.startState=startState;
	}
	
	/**
	 * Add a transition
	 * @param state Preconditional state
	 * @param input On this input
	 * @param newState Change into this state
	 */
	public void addTransition(S state, I input, S newState) {
		if(transitions.get(state) == null)
			transitions.put(state, new HashMap<I,S>());
		transitions.get(state).put(input, newState);
	}
	
	/**
	 * Read an input element and change state accordingly
	 * <p>Does nothing and returns <code>null</code> if no applicable 
	 * transition is found. 
	 * @param input
	 * @return
	 */
	public S step(I input) {
		if(transitions.get(state)!=null &&
				transitions.get(state).get(input) != null) {
			state=transitions.get(state).get(input);
			return state;
		}
		return null;
	}
	
	/**
	 * Get the currents state of the machine
	 * @return The state atm
	 */
	public S getState() {
		return state;
	}
	
	/**
	 * Set the machine back to its starting state 
	 * <p>Useful for reusing machines. 
	 */
	public void reset() {
		this.state=startState;
	}
	
}
