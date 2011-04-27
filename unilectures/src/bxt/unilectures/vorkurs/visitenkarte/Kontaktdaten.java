package bxt.unilectures.vorkurs.visitenkarte;

@CliPrompt("Geben sie nachfolgend die Kontaktdaten ein. ")
public class Kontaktdaten {
	private String vorname=null;
	private String nachname=null;
	private String profession=null;
	private String strasse=null;
	private Integer hausnummer=null;
	private String wohnort=null;
	private Integer plz=null;
	private String mailadresse=null;
	private Phonenumber handynummer=null;
	public String getVorname() {
		return vorname;
	}
	public void setVorname(String vorname) {
		this.vorname = vorname;
	}
	public String getNachname() {
		return nachname;
	}
	public void setNachname(String nachname) {
		this.nachname = nachname;
	}
	public String getProfession() {
		return profession;
	}
	public void setProfession(String profession) {
		this.profession = profession;
	}
	public String getStrasse() {
		return strasse;
	}
	public void setStrasse(String strasse) {
		this.strasse = strasse;
	}
	public Integer getHausnummer() {
		return hausnummer;
	}
	public void setHausnummer(Integer hausnummer) {
		this.hausnummer = hausnummer;
	}
	public String getWohnort() {
		return wohnort;
	}
	public void setWohnort(String wohnort) {
		this.wohnort = wohnort;
	}
	public Integer getPlz() {
		return plz;
	}
	public void setPlz(Integer plz) {
		this.plz = plz;
	}
	public String getMailadresse() {
		return mailadresse;
	}
	public void setMailadresse(String mailadresse) {
		this.mailadresse = mailadresse;
	}
	public Phonenumber getHandynummer() {
		return handynummer;
	}
	public void setHandynummer(Phonenumber handynummer) {
		this.handynummer = handynummer;
	}
	
	
}
