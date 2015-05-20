package bxt.unilectures.algogis.leastsquares;

import java.util.Arrays;
import java.util.Locale;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import Jama.Matrix;

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
		
		LeastSquaresAdjustment l = m.getLeastSquaresAdjustment();
		
		System.out.println("Least-squares estimates of heights:");
		System.out.println(matrixToString(l.getUnknowns()));
		
		System.out.println("Corrected measurements:");
		System.out.println(matrixToString(l.getTrueObservations()));
		
		System.out.println("Standard deviation of heights:");
		System.out.println(IntStream.range(0, m.getPointsSize()-1).mapToObj(i -> ""+Math.sqrt(l.getUnknownVariance().get(i, i))).collect(Collectors.joining("\n")));
		
		System.out.println("Standard deviation of measurements:");
		System.out.println(IntStream.range(0, m.getMeasurementsSize()).mapToObj(i -> ""+Math.sqrt(l.getObservationVariance().get(i, i))).collect(Collectors.joining("\n")));
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
	
	private static String matrixToString(Matrix m) {
		return Arrays
				.stream(m.getArray())
				.map(row -> Arrays.stream(row)
						.mapToObj(v -> ""+v).collect(Collectors.joining(" ")))
						.collect(Collectors.joining("\n"));
	} 
	
}
