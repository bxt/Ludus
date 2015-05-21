package bxt.unilectures.algogis.leastsquares;

import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.Random;
import java.util.stream.IntStream;

public class Experiments {
	
	private static final DecimalFormat DF = new DecimalFormat("0.0000");
	
	private enum RandomMode {UNIFORM, BIASED, EVEN};
	
	public static void main(String[] args) {
		int pointsSize = 15;
		int measurementsSize = 2000;
		double standardDeviation = 30;
		boolean hintVariance = true;
		RandomMode randomMode = RandomMode.BIASED;

		Random random = new Random();
		
		int[] pointHeights = random.ints(pointsSize, 0, 100).toArray();
		pointHeights[0] = 0;
		System.out.println("Acutal heights:");
		System.out.println(Arrays.toString(pointHeights));
		
		double[] noise = IntStream.range(0, measurementsSize).mapToDouble(i -> random.nextGaussian()*standardDeviation).toArray();
		
		Measurements measurements = new Measurements(pointsSize);
		IntStream.range(0, measurementsSize).forEach(i -> {
			int from, to;
			switch(randomMode) {	
			case UNIFORM:
				from = random.nextInt(pointsSize);
				do to = random.nextInt(pointsSize); while (to == from);
				break;
			case BIASED:
				from = Math.min(pointsSize-1, Math.abs((int)(random.nextGaussian()*4)));
				do to = Math.min(pointsSize-1, Math.abs((int)(random.nextGaussian()*4))); while (to == from);
				//System.out.println(String.format("%d:%d", from, to));
				break;
			case EVEN:
				// TODO: find a formula for this calculation
				int x = i % ((pointsSize*(pointsSize-1))/2);
				from = 0;
				for (int k = pointsSize-1; x >= k; k--) {
					from++;
					x -= k;
				}
				to = 1 + from + x;
				break;
			default:
				throw new IllegalStateException();
			}
			
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
