package bxt.unilectures.algorithmenunddatenstrukturen.fun;

/**
 * Hirsch-Index-berechnung (Übung 3, Aufgabe 3)
 * @author Bernhard Häussner et al.
 */
public class HirschIndex {
	
	public static int hirschIndex(int[] citations) {
		// Zu Beginn sortieren
		sort(citations);
		
		// Für jede Publikation speichern, wie oft sie zitiert wurde
		int[] anzahlen=new int[citations.length];
		
		// Da die Aufsatznummern frei vergeben werden können, hier eine
		// (kleine) fortlaufende Nummer vergeben
		int aufsatznummer=0;
		
		// Zählen, wie oft jeder Aufsatz vorkommt
		// (funktioniert durch sortierung)
		for(int i=0;i<citations.length;i++) {
			// Wenn eine andere Nummer kommt, Aufsatznummer hochzählen
			if(i>0&&citations[i]!=citations[i-1]) aufsatznummer++;
			// Aufsatz zählen:
			anzahlen[aufsatznummer]++;
		}
		// Wiederum sortieren
		sort(anzahlen);
		
		// Anzahlen durchgehen
		int index=0;
		for(int i=0;i<citations.length;i++) {
			if(anzahlen[i]>=i+1)
				// Wenn bsp. an 3. Stelle eine 3 steht, gibt es durch
				// die Sortierung mindestens 3 Ausätze mit 3 Zitierungen
				index=i+1;
		}
		// Am Ende steht in index der größte mögliche Hirsch-Index
		
		return index;
	}
	
	// Sortieralgorithmus mit O(n²)
	public static void sort(int[] a) {
		for (int j = 1; j < a.length; j++) {
			int key = a[j];
			int i = j-1;
			while (i >= 0 && a[i] < key ) {
				a[i+1] = a[i];
				i--;
			}
			a[i+1] = key;
		}
	}
	
	public static void main(String... args) {
		int[] citations={1,3,5,4,5,4,2};
		System.out.println(hirschIndex(citations));
	}
}
