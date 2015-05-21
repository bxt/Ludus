package bxt.unilectures.algogis.leastsquares;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Random;
import java.util.stream.IntStream;

public class Experiments {
	
	private static final DecimalFormat DF = new DecimalFormat("0.0000");
	
	public static void main(String[] args) {
		int pointsSize = 5;
		int measurementsSize = 20;
		double standardDeviation = 3;
		boolean hintVariance = true;

		Random random = new Random();
		
		int[] pointHeights = random.ints(pointsSize, 0, 100).toArray();
		pointHeights[0] = 0;
		System.out.println("Acutal heights:");
		System.out.println(Arrays.toString(pointHeights));
		
		Measurements measurements = new Measurements(pointsSize);
		double[] noise = IntStream.range(0, measurementsSize).mapToDouble(i -> random.nextGaussian()*standardDeviation).toArray();
		IntStream.range(0, measurementsSize).forEach(i -> {
			int from = random.nextInt(pointsSize);
			int to;
			do to = random.nextInt(pointsSize); while (to == from);
			double value = pointHeights[to] - pointHeights[from] + noise[i]; 
			double variance = hintVariance ? 1/(noise[i]*noise[i]) : 1;
			measurements.addMeasurement(from + 1, to + 1, value, variance);
		});
		
		LeastSquaresAdjustment l = measurements.getLeastSquaresAdjustment();
	
		System.out.println("Calculated heights:");
		IntStream.range(0, pointsSize-1).forEach(i -> {
			System.out.println(DF.format(l.getUnknowns().get(i, 0)) +  " ± " + DF.format(Math.sqrt(l.getUnknownVariance().get(i, i))));
		});
		
		System.out.println("Measurements:");
		l.getTrueObservations();
		IntStream.range(0, measurementsSize).forEach(i -> {
			Measurement m = measurements.getMeasurements().get(i);
			int correctMeasurement = pointHeights[m.getTo()-1] - pointHeights[m.getFrom()-1];
			System.out.println(String.format("[%d:%d] %d %s %s ->\t %s ± %s"
					, m.getFrom()
					, m.getTo()
					, correctMeasurement
					, (noise[i] < 0 ? "-" : "+")
					, DF.format(Math.abs(noise[i]))
					, DF.format(l.getTrueObservations().get(i, 0))
					, DF.format(Math.sqrt(l.getObservationVariance().get(i, i)))
					));
		});
		
		System.out.println("Overall estimated standard deviation:");
		System.out.println(" ±" + DF.format(Math.sqrt(l.getVariance())));
	}
}
