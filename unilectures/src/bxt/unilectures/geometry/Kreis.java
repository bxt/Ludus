package bxt.unilectures.geometry;

public class Kreis {
    public double xM;
    public double yM;
    public double radius;
    public Kreis(double xM, double yM, double radius) {
    	this.xM=xM;
    	this.yM=yM;
    	this.radius=radius;
    }
    public double flaeche() {
    	return Math.PI*radius*radius;
    }
    public double umfang() {
    	return Math.PI*2*radius;
    }
    /**
     * Berechnet die Distanz zweier Kreise
     * 
     * <p>Berechnet wird die Entfernung der Mittelpunkte minus die Summe der
     * Radii. Dieser Wert ist genau dann echt negativ, wenn die Kreise 
     * ueberlappen, in diesem Fall wird 0 zurueckgegeben, ansonsten kann der Wert 
     * als Ergebnis zurueckgegeben werden. </p>
     * 
     * <p><i>Ueberlappen := Es existiert ein gemeinsamer Punkt, welcher nicht auf
     * einer Kante liegt. </i></p>
     * 
     * @param k Der andere Kreis
     * @return Die Entfernung
     */
    public double distanz(Kreis k) {
    	double dist=Math.sqrt((xM-k.xM)*(xM-k.xM)+(yM-k.yM)*(yM-k.yM))
    		-radius-k.radius;
    	if(dist<0) return 0.0d;
    	return dist;
    }
}
