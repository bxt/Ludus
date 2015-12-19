package bxt.unilectures.oop.patterns.composite;

public class CircuitTest {
	public static void main(String[] args) {
		CircuitCollecion parallel = new ParallelCircuit();
		
		CircuitCollecion series = new SeriesCircuit();
		series.add(new Resistor(10));
		series.add(new Resistor(20));
		
		parallel.add(series);
		parallel.add(new Resistor(40));
		
		System.out.println(parallel.getResistorCount()); // 3
		System.out.println(parallel.getResistance()); // about 17
		
	}
}
