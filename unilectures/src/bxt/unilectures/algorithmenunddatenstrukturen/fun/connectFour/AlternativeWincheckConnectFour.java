package bxt.unilectures.algorithmenunddatenstrukturen.fun.connectFour;


/**
 * Vier-Gewinnt
 * @author Bernhard Häussner
 * @date 2011-10-24
 */
public class AlternativeWincheckConnectFour {
	
	/**
	 * Auf Sieg prüfen
	 * @param field Siehe {@link #makeMove(char[][], char, int)}
	 * @param player 'O' oder 'X'
	 * @return true, wenn player das Kriterium fuer einen Sieg erfuellt
	 */
	public static boolean hasWon(char[][] field, char player) {
		for(int zeile=0;zeile<field.length;zeile++) {
			for(int spalte=0;spalte<field[zeile].length;spalte++) {
				if(field[zeile][spalte]==player) {
					if(
							hasWonInner(field,player,zeile,spalte, 1, 0) ||
							hasWonInner(field,player,zeile,spalte, 1, 1) ||
							hasWonInner(field,player,zeile,spalte, 0, 1) ||
							hasWonInner(field,player,zeile,spalte,-1, 0) ||
							hasWonInner(field,player,zeile,spalte,-1,-1) ||
							hasWonInner(field,player,zeile,spalte, 0,-1) ||
							hasWonInner(field,player,zeile,spalte, 1,-1) ||
							hasWonInner(field,player,zeile,spalte,-1, 1)
							) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	private static boolean hasWonInner(char[][] field, char player, int zeile, int spalte, int stepZ, int stepS) {
		int count=0;
		while(field[zeile][spalte]==player) {
			zeile+=stepZ;
			spalte+=stepS;
			count++;
			if(
					zeile<0 || 
					spalte <0 || 
					zeile>=field.length || 
					spalte>=field[0].length)
				return false;
			if(count>=ConnectFour.WINCNT)
				return true;
		}
		return false;
	}

}
