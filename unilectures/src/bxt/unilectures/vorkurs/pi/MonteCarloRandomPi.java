package bxt.unilectures.vorkurs.pi;

/**
 * Calculate Pi via Monte Carlo integration over a circle using pseudo random points
 * @author bxt
 */
public class MonteCarloRandomPi extends MonteCarloPi {

	@Override
	protected Point generatePoint(int i,int total) {
		return new Point(Math.random(),Math.random());
	}

}
