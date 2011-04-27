/**
 * 
 */
package bxt.unilectures.vorkurs.visitenkarte;

/**
 * Generiert Visitenkarten
 * @author bxt
 * @version 1.0
 * @date 2011-04-27
 */
public class Visitenkarte {

	/**
	 * Prompt for some contact information and render a neat ASCII business card
	 * @param args CLI parameters (unused)
	 */
	public static void main(String[] args) {
		CliInputFactory builder= new CliInputFactory();
		Kontaktdaten k=builder.create(Kontaktdaten.class);
		KontaktdatenRenderer r= new KontaktdatenRenderer();
		r.render(k);
	}

}
