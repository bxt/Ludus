package bxt.unilectures.vorkurs.visitenkarte;

/**
 * A (very poor) bean for a phone number
 * @author bxt
 */
@CliPrompt("")
public class Phonenumber {
	private String phonenumber=null;

	public void setPhonenumber(String phonenumber) {
		this.phonenumber = phonenumber;
	}

	public String getPhonenumber() {
		return phonenumber;
	}
}
