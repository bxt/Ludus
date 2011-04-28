package bxt.unilectures.vorkurs.pi;

/**
 * Calculate Pi using Leibniz formula
 * Calculates ( +1/1 -1/3 +1/5 -1/7 +1/9 ... )*4
 * @author bxt
 */
public class LeibnizRowPi implements PiStrategy {
	@Override
	public double getPi(int iterations) {
		boolean isNextAddition = true;
		int nextDivisor=1;
		double pi=0;
		for (;iterations>0;iterations--) {
			pi+=1.0/nextDivisor*(isNextAddition?1:-1);
			isNextAddition=!isNextAddition;
			nextDivisor+=2;
		}
		return pi*4.0;
	}
}
