package bxt.unilectures.informationsuebertragung.fun.redundancy;

import java.util.LinkedList;
import java.util.List;

import bxt.unilectures.informationsuebertragung.fun.staging.Stage;
import bxt.unilectures.informationsuebertragung.fun.staging.Tuple;


public class Redundancy implements Stage<List<List<Boolean>>,Tuple<Blockcode,List<List<Boolean>>>> {
	
	public static void main(String[] args) {
		
		MatrixZ2 p = MatrixZ2.fromString("1110100 1101010 1011001");
		
		MatrixZ2 g = MatrixZ2.catColumns(
				MatrixZ2.unit(4), 
				MatrixZ2.transpose(p.grepColumns(0, 4)));
		
		System.out.println(g);
		
		MatrixZ2 nutz = MatrixZ2.fromString("1010");
		
		MatrixZ2 cw = MatrixZ2.mult(nutz, g);
		
		System.out.println(cw);
		
		MatrixZ2 syndorme=MatrixZ2.mult(p, MatrixZ2.transpose(cw));
		
		System.out.println(syndorme.equals(MatrixZ2.zero(3,1)));
		
	}

	@Override
	public Tuple<Blockcode, List<List<Boolean>>> tick(List<List<Boolean>> input) {
		
		Blockcode code=Blockcode.fromPolynomial(MatrixZ2.fromString("111010001"),7);
		
		List<List<Boolean>> output = new LinkedList<List<Boolean>>();
		
		for(List<Boolean> vector : input) {
			MatrixZ2 codeword = code.getCodeword(
					MatrixZ2.transpose(MatrixZ2.fromVector(vector)));
			output.add(codeword.toBitvector());
		}
		
		System.out.println("Bits with useful redundancy:");
		System.out.println(booleanBlocksToString(output));
		
		return new Tuple<Blockcode, List<List<Boolean>>>(code, output);
	}

	@Override
	public List<List<Boolean>> tock(Tuple<Blockcode, List<List<Boolean>>> input) {
		Blockcode code=input.getA();
		 List<List<Boolean>> inputV=input.getB();
		
		System.out.println("Possibliy broken bits:");
		System.out.println(booleanBlocksToString(inputV));
		
		List<List<Boolean>> output = new LinkedList<List<Boolean>>();
		
		for(List<Boolean> vector : inputV) {
			MatrixZ2 codeword=MatrixZ2.transpose(MatrixZ2.fromVector(vector));
			if(!code.checkCodeword(codeword))
				throw new RuntimeException("Doh!");
			output.add(code.revealCodeword(codeword).toBitvector());
		}
		
		return output;
	}
	
	/**
	 * Helper method to print a boolean block list as <code>0100 1111 ...</code>
	 * @param list The block list to display
	 * @return A string of zeros, ones and spaces
	 */
	private static String booleanBlocksToString(List<List<Boolean>> list) {
		StringBuffer sb = new StringBuffer();
		for(List<Boolean> block : list) {
			for(Boolean b : block) 
				sb.append(b==Boolean.FALSE?'0':'1');
			sb.append(' ');
		}
		return sb.toString();
	}
}
