package bxt.unilectures.oop.patterns.composite;

public class ParallelCircuit extends CircuitCollecion {

	@Override
	public double getResistance() {
		return 1 / getCircuits().stream().mapToDouble(Circuit::getResistance).map((d) -> 1/d).sum();
	}

}
