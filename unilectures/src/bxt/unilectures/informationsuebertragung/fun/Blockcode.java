package bxt.unilectures.informationsuebertragung.fun;

import java.util.List;

public class Blockcode {
	
	private MatrixZ2 generator;
	private MatrixZ2 checker;
	
	private Blockcode(MatrixZ2 generator, MatrixZ2 checker) {
		this.generator = generator;
		this.checker = checker;
	}

	public MatrixZ2 getCodeword(MatrixZ2 input) {
		return MatrixZ2.mult(input,generator);
	}
	
	public boolean checkCodeword(MatrixZ2 codeword) {
		
		MatrixZ2 syndorme=MatrixZ2.mult(checker, MatrixZ2.transpose(codeword));
		
		return syndorme.equals(MatrixZ2.zero(syndorme.rows(),1));
	}
	
	public static Blockcode fromGenerator(MatrixZ2 generator) {
		generator.gaussJordan();
		int n=generator.columns();
		int m=generator.rows();
		int k=n-m;
		MatrixZ2 checker=MatrixZ2.catColumns(
				MatrixZ2.transpose(generator.grepColumns(m, n)),
				MatrixZ2.unit(k));
		return new Blockcode(generator, checker);
	}
	
	public static Blockcode fromPolynomial(MatrixZ2 polimomial, int m) {
		int mR=m-1;
		MatrixZ2 generator=MatrixZ2.catColumns(polimomial, MatrixZ2.zero(1, mR));
		while (mR>0) {
			mR--;
			generator=MatrixZ2.catRows(generator, 
					MatrixZ2.catColumns(MatrixZ2.catColumns(
							MatrixZ2.zero(1, m-mR-1), 
							polimomial), 
							MatrixZ2.zero(1, mR)));
		}
		return Blockcode.fromGenerator(generator);
	}
	
	public static int hammingDistance(List<Boolean> a, List<Boolean> b) {
		int hd=0;
		for(int i=0;i<a.size()&&i<b.size();i++) {
			if(a.get(i)!=b.get(i)) hd++;
		}
		return hd;
	}
	
	/**
	 * Construct BCH(15, 7, 5) code and generate a codeword. Then check the 
	 * codeword, the codeword with one corrupted bit, and a by 1 shifted 
	 * codeword for validity. 
	 * <p>
	 * The example values are taken from 
	 * <a href="http://de.wikipedia.org/w/index.php?title=BCH-Code&oldid=96722948">
	 * Wikipedia</a>
	 * @param args
	 */
	public static void main(String[] args) {
		
		// our 
		Blockcode code=fromPolynomial(MatrixZ2.fromString("111010001"),7);
		System.out.println("The generator Matrix:");
		System.out.println(code.generator);
		
		System.out.println("Build and check a codeword:");
		MatrixZ2 codeword = code.getCodeword(MatrixZ2.fromString("1001011"));
		System.out.println(code.checkCodeword(codeword)+":");
		System.out.println(codeword);
		
		System.out.println("Now chenge 4 bits of the codeword:");
		List<Boolean> codevector=codeword.toBitvector();
		codevector.set(1, !codevector.get(1));
		codevector.set(3, !codevector.get(3));
		codevector.set(4, !codevector.get(4));
		codevector.set(7, !codevector.get(7));
		MatrixZ2 badCodeword = 
				MatrixZ2.transpose(MatrixZ2.fromVector(codevector));
		int hd1=hammingDistance(codevector, codeword.toBitvector());
		System.out.println(code.checkCodeword(badCodeword)+", hd "+hd1);
		System.out.println(badCodeword);
		
		System.out.println("Or shift the codeword:");
		List<Boolean> codevector2=codeword.toBitvector();
		Boolean start=codevector2.remove(0);
		codevector2.add(start);
		MatrixZ2 fakeCodeword = 
				MatrixZ2.transpose(MatrixZ2.fromVector(codevector2));
		int hd2=hammingDistance(codevector2, codeword.toBitvector());
		System.out.println(code.checkCodeword(fakeCodeword)+", hd "+hd2);
		System.out.println(fakeCodeword);
		System.out.println("HD:"+hd2);
		
	}
	
}
