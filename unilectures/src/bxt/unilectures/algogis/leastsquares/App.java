package bxt.unilectures.algogis.leastsquares;

import java.util.Arrays;
import java.util.Locale;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import Jama.Matrix;

public class App {
	
	public static void main(String[] args) {
		String input = ""
				+ "4\n"
				+ "5\n"
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
		
		BiFunction<Integer, Matrix, String> printSqrtDiagonal = (n, x) ->
			IntStream.range(0, n)
				.mapToObj(i -> x.get(i, i))
				.map(v -> Double.toString(Math.sqrt(v)))
				.collect(Collectors.joining("\n"));
		
		Stream.of( "Least-squares estimates of heights:"
		         , matrixToString(l.getUnknowns())
		         
		         , "Corrected measurements:"
		         , matrixToString(l.getTrueObservations())
		         
		         , "Standard deviation of heights:"
		         , printSqrtDiagonal.apply(m.getPointsSize()-1, l.getUnknownVariance())
		         
		         , "Standard deviation of measurements:"
		         , printSqrtDiagonal.apply(m.getMeasurementsSize(), l.getObservationVariance())
		         
		         ).forEachOrdered(System.out::println);
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
				.map(row -> Arrays
						.stream(row)
						.mapToObj(Double::toString)
						.collect(Collectors.joining(" ")))
				.collect(Collectors.joining("\n"));
	} 
	
}
