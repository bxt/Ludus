package bxt.unilectures.vorkurs.geometry;

public class Quadrat {
    public double xM;
    public double yM;
    public double seitenlaenge;
    public Quadrat(double xM, double yM, double seitenlaenge) {
    	this.xM=xM;
    	this.yM=yM;
    	this.seitenlaenge=Math.abs(seitenlaenge);
    }
    public double flaeche() {
    	return seitenlaenge*seitenlaenge;
    }
    public double umfang() {
    	return seitenlaenge*4;
    }
    /**
     * Berechnet die Distanz zweier Quadrate
     * 
     * <p>Berechnet zunaechst die Distanz parallel zu den Koordinatenachsen. Ist 
     * diese fuer beide Achsen <0 ueberlappen sich die Quadrate. Ist sie fuer eine
     * Achse <0, ist eine kuerzeste Verbindungsstrecke eine Parallele der
     * anderen Koordinatenachse und ihre Laege die Entfernung parallel zur 
     * anderen Achse, die Quadrate sind "nebeneinander". </p>
     * <p>Tritt keiner dieser Sonderfaelle ein, wird zu den 
     * Mittelpunktkoordinaten jeweils die halbe Seitenlaenge addiert und zwar 
     * mit dem Vorzeichen der jeweiligen Mittelpunktdifferenz. Das funktioniert, 
     * weil der Ueberlappungsfall ausgeschlossen wurde. Es ergeben sich die 
     * beiden Eckpunkte, die die Grenzen der kuerzesten Verbindungsstrecke sind. 
     * Ihre Distanz (Pythagoras) ist die gesuchte Laenge. </p>
      * 
     * <p><i>Ueberlappen := Es existiert ein gemeinsamer Punkt, welcher nicht auf
     * einer Kante liegt. </i></p>
     * 
     * @param q Das andere Quadrat
     * @return Die Entfernung
     */
    public double distanz(Quadrat q) {
    	double gapX=(Math.abs(xM-q.xM)-q.seitenlaenge/2-seitenlaenge/2);
    	double gapY=(Math.abs(yM-q.yM)-q.seitenlaenge/2-seitenlaenge/2);
    	if (gapX<0&&gapY<0){ // they overlap
    		return 0;
    	}
    	if(gapX<0) {
    		return gapY;
    	}
    	if(gapY<0) {
    		return gapX;
    	}
    	// Find coords of the edges nearest to the other square
    	double myEdgeX=(q.xM-xM>0?1:-1)*seitenlaenge/2+xM;
    	double myEdgeY=(q.yM-yM>0?1:-1)*seitenlaenge/2+yM;
    	double otEdgeX=(xM-q.xM>0?1:-1)*q.seitenlaenge/2+q.xM;
    	double otEdgeY=(yM-q.yM>0?1:-1)*q.seitenlaenge/2+q.yM;
    	// distance of these edges
    	return Math.sqrt(
    			(myEdgeX-otEdgeX)*(myEdgeX-otEdgeX)+
    			(myEdgeY-otEdgeY)*(myEdgeY-otEdgeY)
    			);
    }
    /**
     * Benutzt die ueberlappungsfunktion fuer Quadrate (zum testen)
     * @param args
     */
    public static void main(String[] args) {
    	
    	Quadrat q1=new Quadrat(2,2,2);
    	Quadrat q2=new Quadrat(8,9,4);
    	Quadrat q3=new Quadrat(10,4,2);
    	Quadrat q4=new Quadrat(12,9,2);
    	Quadrat q5=new Quadrat(5,12,2);
    	Quadrat q6=new Quadrat(9,10,1);
    	Quadrat q7=new Quadrat(2,7,2);
    	Quadrat q8=new Quadrat(10,11,1);
    	
    	System.out.println("5 "+q1.distanz(q2));
    	System.out.println("5 "+q2.distanz(q1));
    	
    	System.out.println("2 "+q3.distanz(q2));
    	System.out.println("2 "+q2.distanz(q3));
    	
    	System.out.println("1 "+q4.distanz(q2));
    	System.out.println("1 "+q2.distanz(q4));
    	
    	System.out.println("0 "+q5.distanz(q2));
    	System.out.println("0 "+q2.distanz(q5));
    	
    	System.out.println("0 "+q6.distanz(q2));
    	System.out.println("0 "+q2.distanz(q6));
    	
    	System.out.println("3 "+q7.distanz(q2));
    	System.out.println("3 "+q2.distanz(q7));
    	
    	System.out.println("0 "+q8.distanz(q2));
    	System.out.println("0 "+q2.distanz(q8));
    	
    }
}
