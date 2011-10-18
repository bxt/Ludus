package connectFour;

import java.util.Scanner;

/**
 * Vier-Gewinnt
 * @author Bernhard Häussner, 1797901
 * @date 2011-10-18
 */
public class ConnectFour {
	/**
	 * Spielzug ausgefuhen, das Spielgitter field aktualisieren. 
	 * 
	 * @param field Referenz auf Spielgitter, Spielsteine verschiedener Farben 
	 *   werden durch die Buchstaben 'X' und 'O' repraesentiert, leere Zellen
	 *   durch das Leerzeichen ' ', field[0][0] ist die linke untere Ecke. 
	 * @param player 'X' oder 'O', je nachdem, welche Farbe am Zug ist.
	 * @param column Gibt an, in welche Spalte der Spielstein geworfen wird.
	 * @return Genau dann false wenn der Spielzug nicht ausgeubt werden konnte.
	 */
	public static boolean makeMove(char[][] field, char player, int column) {
		// Gueltigkeitstest
		if(column<0 || column >= field[0].length) return false; // aussen
		if(field[field.length-1][column]!=' ') return false; // voll
		
		// Erste leere Zelle auswaehlen: 
		int height=0;
		while (field[height][column]!=' ') height++;
		
		// Speilstein in Zelle setzen:
		field[height][column]=player;
		
		return true;
	}
	
	/**
	 * Spielgitter field auf den Bildschirm ausgeben
	 * @param field Siehe {@link #makeMove(char[][], char, int)}
	 */
	public static void printField(char[][] field) {
		// Zeilen rückwärts, dann Spalten, ausgeben:
		for(int zeile=field.length-1;zeile>=0;zeile--) {
			System.out.print('|');
			for(int spalte=0;spalte<field[zeile].length;spalte++) {
				System.out.print(field[zeile][spalte]);
			}
			System.out.println('|');
		}
		
		// Spaltennamen zur orientierung:
		System.out.print(' ');
		for(int spalte=0;spalte<field[0].length;spalte++) {
			System.out.print(spalte);
		}
		System.out.println(' ');
	}
	
	/**
	 * Uberpruefen, ob im Spielgitter field alle Zellen belegt sind. 
	 * @param field Siehe {@link #makeMove(char[][], char, int)}
	 * @return Wenn dies der Fall ist, true, sonst false.
	 */
	public static boolean isFull(char[][] field) {
		boolean returnValue=true;
		int zeile=field.length-1; // obere zeile
		for(int spalte=0;spalte<field[zeile].length;spalte++) {
			returnValue = returnValue && // logisches und über alle Spalten
				field[zeile][spalte]!=' '; // voll?
		}
		return returnValue;
	}
	
	/**
	 * Diese Methode wird beim Start des Programms ausgefuhrt. 
	 * <p>
	 * Die SpielerInnen werden zunaechst darum gebeten werden, die Dimensionen 
	 * m und n des Spielgitters einzugeben. Danach werde sie abwechselnd nach 
	 * ihrem Zug gefragt. Ist ein Zug ungultig, so muss er wiederholt werden. 
	 * Nach jedem Zug wird das Spielgitter ausgeben.  Falls das Spielgitter 
	 * voll ist, wird das Programm beendet. 
	 * @param args
	 */
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		
		// Nach Dimensionen m und n des Spielgitters fragen:
		System.out.println("Breite des Feldes:");
		int width= sc.nextInt();

		System.out.println("Hoehe des Feldes:");
		int height= sc.nextInt();
		
		// Feld mit Leerzeichen initialisieren:
		char[][] field = new char[height][width];
		for(int zeile=0;zeile<field.length;zeile++) {
			for(int spalte=0;spalte<field[zeile].length;spalte++) {
				field[zeile][spalte]=' ';
			}
		}
		
		char[] players={'X','O'};
		for(int zugnummer=0;!isFull(field);zugnummer++) {
			// Speieler abwechselnd auswaehlen:
			char currentPayer=players[zugnummer%players.length];
			
			int column = 0;
			do { // wenn der Zug ungueltig war, wiederholen
				System.out.println("Spieler "+currentPayer+" Spalte eingeben:");
				column= sc.nextInt();
			} while (!makeMove(field, currentPayer, column));
			printField(field);
		}
	}
}
