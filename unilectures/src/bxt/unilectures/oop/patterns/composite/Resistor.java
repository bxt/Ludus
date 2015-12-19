package bxt.unilectures.oop.patterns.composite;

public class Resistor implements Circuit {
	
	private double resistance;
	
	public Resistor(double resistance) {
		this.resistance = resistance;
	}

	@Override
	public int getResistorCount() {
		return 1;
	}

	@Override
	public double getResistance() {
		return resistance;
	}

}
