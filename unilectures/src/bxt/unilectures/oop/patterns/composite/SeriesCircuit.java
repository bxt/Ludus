package bxt.unilectures.oop.patterns.composite;

public class SeriesCircuit extends CircuitCollecion {

	@Override
	public double getResistance() {
		return getCircuits().stream().mapToDouble(Circuit::getResistance).sum();
	}

}
