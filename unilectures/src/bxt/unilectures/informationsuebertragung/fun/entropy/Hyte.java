package bxt.unilectures.informationsuebertragung.fun.entropy;

import java.util.ArrayList;
import java.util.List;

/**
 * Objects to represent hytes, i.e. 4 bits, half a byte or 1 hex digt 
 */
public enum Hyte {
	// Here they are, named Hx where x is the hex digit
	H0,H1,H2,H3,H4,H5,H6,H7,H8,H9,HA,HB,HC,HD,HE,HF;
	
	/**
	 * Converts a byte array into a list of hytes
	 * @param input A field of bytes
	 * @return A list of hytes, twice the length
	 */
	public static List<Hyte> fromByteArray(byte[] input) {
		List<Hyte> result = new ArrayList<Hyte>();
		for(byte b : input) {
			result.add(Hyte.values()[(b & 0xf0) >> 4 ]);
			result.add(Hyte.values()[b & 0x0f]);
		}
		return result;
	}
	
	/**
	 * Converts a list of hytes into a byte array
	 * @param input A list of hytes
	 * @return A field of bytes, half the length
	 */
	public static byte[] toByteArray(List<Hyte> input) {
		if(input.size()%2!=0) return null;
		byte[] result=new byte[input.size()/2];
		for(int i=0;i<input.size();i++) {
			if(i%2==0) {
				result[i/2]=(byte) (input.get(i).ordinal() << 4);
			} else {
				result[i/2]=(byte) (result[i/2]+input.get(i).ordinal());
			}
		}
		return result;
	}
	
}
