package bxt.unilectures.oop.patterns.composite;

import java.util.ArrayList;
import java.util.List;

public abstract class CircuitCollecion implements Circuit {
	
	private List<Circuit> circuits = new ArrayList<>();
	
	public void add(Circuit circuit) {
		circuits.add(circuit);
	}
	
	public void remove(Circuit circuit) {
		circuits.add(circuit);
	}
	
	@Override
	public int getResistorCount() {
		return circuits.stream().mapToInt(Circuit::getResistorCount).sum();
	}
	
	protected List<Circuit> getCircuits() {
		return circuits;
	}
}
