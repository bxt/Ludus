package bxt.unilectures.vorkurs.pi;

public class LeibnizRowPi implements piStrategy {
	/* (non-Javadoc)
	 * @see bxt.unilectures.vorkurs.pi.piStrategy#getPi(int)
	 */
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
