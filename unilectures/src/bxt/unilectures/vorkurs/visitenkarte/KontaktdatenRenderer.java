package bxt.unilectures.vorkurs.visitenkarte;

import java.io.PrintStream;

/**
 * A class rendering the information contained by {@link Kontaktdaten} as neat ASCII business card
 * @author bxt
 */
public class KontaktdatenRenderer extends BoxBuilder {
	/**
	 * Stream to render business card into
	 */
	private PrintStream out=null;
	/**
	 * A no-arg constructor using sensible default values
	 */
	KontaktdatenRenderer() {
		this(40,System.out);
	}
	/**
	 * Constructor for full control
	 * @param width Inner width of the business card, without border an padding 
	 * @param out Stream to render business card into
	 */
	KontaktdatenRenderer(int width,PrintStream out) {
		this.width=width;
		this.out=out;
	}
	/**
	 * Does the actual render with the preferences set in the constructor
	 * @param k The data to use
	 */
	public void render(Kontaktdaten k) {
		this.outStr=new StringBuilder();
		renderLineBorder();
		renderLineLeftAligned("");
		renderLineLeftAligned(k.getNachname()+", "+k.getVorname());
		renderLineLeftAligned(k.getProfession());
		renderLineLeftAligned("");
		renderLineLeftAligned(k.getStrasse()+" "+k.getHausnummer());
		renderLineLeftAligned(k.getPlz()+" "+k.getWohnort());
		renderLineLeftAligned("");
		renderLineRightAligned("eMail: "+k.getMailadresse()+"");
		renderLineRightAligned("Handy: "+k.getHandynummer().getPhonenumber()+"");
		renderLineLeftAligned("");
		renderLineBorder();
		out.print(outStr);
	}
}
