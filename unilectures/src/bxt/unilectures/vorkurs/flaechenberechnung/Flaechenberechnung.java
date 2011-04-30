package bxt.unilectures.vorkurs.flaechenberechnung;

public class Flaechenberechnung {
	/**
	 * Flaechenberechnung mittels Gaussscher Trapezformel
	 * @param p Polygon (einfach)
	 * @return Flaeche
	 */
    public static double flaeche(Polygon p) {
    	double a=0;
    	for (int i=0;i<p.punkte.length;i++) {
    		a+=(
    				getPunkt(p,i).y-getPunkt(p,i+1).y)*
    				(getPunkt(p,i).x+getPunkt(p,i+1).x);
    	}
    	return Math.abs(a/2);
    }
    /**
     * Bestimmung, ob ein Polygon einfach ist mittels Innenwinkelsumme
     */
    public static boolean istEinfach(Polygon p) {
    	return innenwinkelsumme(p)==(p.punkte.length-2)*180;
    }
    public static double innenwinkelsumme(Polygon p) {
    	double innenWS=0;
    	for (int i=0;i<p.punkte.length;i++) {
    		innenWS+=winkel(getPunkt(p,i-1),getPunkt(p,i),getPunkt(p,i+1));
    	}
    	return innenWS;
    }
    /**
     * Winkel (gerichtet) aus 3 Punkten
     * @param a Punkt auf erstem Schenkel
     * @param center Schnittpunkt der Schenkel
     * @param b Punkt auf zweitem Schenkel
     * @return
     */
    private static double winkel(Punkt a,Punkt center,Punkt b) {
    	double atan2b=Math.atan2(b.y-center.y, b.x-center.x)/Math.PI*180;
    	double atan2a=Math.atan2(a.y-center.y, a.x-center.x)/Math.PI*180;
    	double result=atan2a-atan2b;
    	return (result+360)%360;
    }
    /**
     * Einen Polygon-Punkt mit Modulo-Funktion finden
     * @param p
     * @param i
     * @return
     */
    private static Punkt getPunkt(Polygon p,int i) {
    	while (i<0) i+=p.punkte.length;
    	return p.punkte[i%p.punkte.length];
    }
    /**
     * Zum Testen
     * @param args CLI-Argumente
     */
    public static void main(String [] args) {
    	Punkt[] punkte={
    			new Punkt(3,1),
     			new Punkt(5,2),
    			new Punkt(3,3),
    			new Punkt(2,4),
    			new Punkt(1,1)};
    	Polygon p= new Polygon(punkte);
    	System.out.print(istEinfach(p)?"einfach":"ueberschlagen");
    	System.out.println(", A: "+flaeche(p));
    	
    	Punkt[] punkte1={
    			new Punkt(0,0),
    			new Punkt(2,0),
    			new Punkt(2.5,1),
    			new Punkt(0.5,2)};
    	Polygon p1= new Polygon(punkte1);
    	System.out.print(istEinfach(p1)?"einfach":"ueberschlagen");
    	System.out.println(", A: "+flaeche(p1));
   }
}
