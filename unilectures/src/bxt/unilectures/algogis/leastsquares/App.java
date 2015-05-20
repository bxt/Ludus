package bxt.unilectures.algogis.leastsquares;

import java.util.Arrays;
import java.util.Locale;
import java.util.Scanner;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import Jama.Matrix;
import bxt.unilectures.algogis.leastsquares.Measurements.Measurement;

public class App {
	
	
	public static void main(String[] args) {
		String input = ""
				+ "4\n"
				+ "5\n" // + "6\n" // probably wrong in assignment
				+ "1 2 4.1 1\n"
				+ "2 3 -7 1\n"
				+ "3 4 1.1 1\n"
				+ "4 1 1.2 1\n"
				+ "4 2 5.4 1\n"
				+ "\n"
				+ "\n"
				+ "\n"
				;
		Measurements m = scanMeasurements(new Scanner(input));
		
		Matrix observations = buildObservations(m);
		Matrix phi = buildPhi(m);
		 double[] variance = buildVariance(m);
		
		LeastSuqaresAdjustment l = new LeastSuqaresAdjustment(observations, phi, variance);
		System.out.println("Least-squares estimates of heights:");
		System.out.println(matrixToString(l.getUnknowns()));
		System.out.println("Corrected measurements:");
		System.out.println(matrixToString(l.getTrueObservations()));
	}

	private static Measurements scanMeasurements(Scanner scanner) {
		scanner.useLocale(Locale.US);
		int pointsSize = scanner.nextInt();
		int maseurementSize = scanner.nextInt();
		
		Measurements maseurements =  new Measurements(pointsSize);
		
		IntStream.range(0, maseurementSize).forEach(mn -> {
			int from = scanner.nextInt();
			int to = scanner.nextInt();
			double value = scanner.nextDouble();
			double variance = scanner.nextDouble();
			maseurements.addMeasurement(from, to, value, variance);
		});
		
		return maseurements;
	}
	
	private static Matrix buildObservations(Measurements m) {
		return new Matrix(m.getMeasurements().stream().map(v -> new double[]{v.getValue()}).toArray(size -> new double[size][]));
	}
	
	private static double[] buildVariance(Measurements m) {
		return m.getMeasurements().stream().mapToDouble(Measurement::getVariance).toArray();
	}
	
	private static Matrix buildPhi(Measurements m) {
		Matrix result = new Matrix(m.getMeasurementsSize(), m.getPointsSize()-1);
		buildPhiHelper(result, m, Measurement::getFrom, -1);
		buildPhiHelper(result, m, Measurement::getTo  ,  1);
		return result;
	}
	
	private static void buildPhiHelper(Matrix result, Measurements m, ToIntFunction<Measurement> f, double value) {
		for(int i = 0; i < m.getMeasurementsSize(); i++) {
			int point = f.applyAsInt(m.getMeasurements().get(i));
			if(point != 1)
				result.set(i, point - 2, value);
		}
	}
	
	private static String matrixToString(Matrix m) {
		return Arrays.stream(m.getArray()).map(row -> Arrays.stream(row).mapToObj(v -> ""+v).collect(Collectors.joining(" "))).collect(Collectors.joining("\n"));
	} 
	
}
