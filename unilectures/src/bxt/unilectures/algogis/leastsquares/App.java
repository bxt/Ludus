package bxt.unilectures.algogis.leastsquares;

import java.util.Locale;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import Jama.Matrix;

public class App {
	
	private static boolean USE_STDIN = false;
	
	private static String EXAMPLE_INPUT = ""
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
	
	public static void main(String[] args) {
		Scanner sc = USE_STDIN ? new Scanner(System.in) : new Scanner(EXAMPLE_INPUT);
		
		Measurements m = scanMeasurements(sc);
		
		LeastSquaresAdjustment l = m.getLeastSquaresAdjustment();
		
		printResults(m, l);
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
	
	private static void printResults(Measurements m, LeastSquaresAdjustment l) {
		Stream.of( "Least-squares estimates of heights:"
		         , Util.matrixToString(l.getUnknowns())
		         
		         , "Corrected measurements:"
		         , Util.matrixToString(l.getTrueObservations())
		         
		         , "Standard deviation of heights:"
		         , stringSqrtDiagonal(m.getPointsSize()-1, l.getUnknownVariance())
		         
		         , "Standard deviation of measurements:"
		         , stringSqrtDiagonal(m.getMeasurementsSize(), l.getObservationVariance())
		         
		         ).forEachOrdered(System.out::println);
	}
	
	private static String stringSqrtDiagonal(int n, Matrix m) {
		return IntStream.range(0, n)
			.mapToObj(i -> m.get(i, i))
			.map(v -> Double.toString(Math.sqrt(v)))
			.collect(Collectors.joining("\n"));
	}
	
}
