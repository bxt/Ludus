package bxt.unilectures.informationsuebertragung.fun;

import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

import bxt.unilectures.informationsuebertragung.fun.Counts.Unit;

public class Main {

	/**
	 * Will run the Huffman compresson
	 * <p>
	 * Will produce informative output while performing the following steps:
	 * <ul>
	 * <li> Convert bytes to hytes and utf-8 string
	 * <li> Count probabilities and dump some facts about them
	 * <li> Calculate a code table from probabilities
	 * <li> Show some facts about the code table
	 * <li> Endoce the hytes to a compressed binary
	 * <li> Display comression ratio
	 * <li> Dedoce the compressed binary to hytes again 
	 * <li> Convert hytes to bytes and utf-8 string
	 * <li> Hope everything stays the same :)
	 * </ul>
	 * @param args
	 */
	public static void main(String[] args) {
		
		Charset charset = Charset.forName("UTF-8");
		
		byte[] input = {0x42, 0x65, 0x72, 0x6E, 0x68, 0x61, 0x72, 
				0x64, 0x20, 0x48, (byte)0xC3, (byte)0xA4, 0x75, 
				0x73, 0x73, 0x6E, 0x65, 0x72};
		
		/*
		// another testing value:
		byte[] input="aaaaaaltaaaaaaaaaaaaaaaaah!".getBytes(charset);
		*/
		
		String textInput = new String(input,charset);
		System.out.println("Data interpreted as UTF-8 text: "+textInput);
		System.out.println("  Size: "+input.length*8+" bits");
		
		List<Hyte> hytes = Hyte.fromByteArray(input);
		
		System.out.println("Chunked into Hytes:"+hytes);
		System.out.println("  Size: "+hytes.size()*4+" Bits");
		
		Counts<Hyte> counts = new Counts<Hyte>(hytes);
		
		System.out.println("  Probability of H6");
		System.out.printf("    %.3f %%\n",
				counts.getProbability(Hyte.H6));
		System.out.println("  Self Information of H6");
		System.out.printf("    %.3f bits\n",
				counts.getOneSelfInformation(Hyte.H6,Unit.BITS));
		System.out.println("  Probability of HC");
		System.out.printf("    %.3f %%\n",
				counts.getProbability(Hyte.HC));
		System.out.println("  Self Information of HC");
		System.out.printf("    %.3f bits\n",
				counts.getOneSelfInformation(Hyte.HC,Unit.BITS));
		
		System.out.println("  Maximum Self Information, equal probabilities");
		System.out.printf("    %.3f bits\n",
				counts.getMaximumSelfInformation(Unit.BITS));
		System.out.println("  Actual Self Information");
		System.out.printf("    %.3f bits\n",
				counts.getSelfInformation(Unit.BITS));
		System.out.println("  Input self information");
		System.out.printf("    %.3f bits\n",
				counts.getSelfInformation(Unit.BITS)*hytes.size() );
		
		System.out.print("\n\n");
		
		CodeStrategy[] codeStrategies = {
				new Huffman(),
				new Block(),
				new ShannonFano() };
		
		List<Hyte> restored = null;
		for(CodeStrategy codeStrategy : codeStrategies) {
		
			System.out.println(String.format("Creating code table with %s...",
					codeStrategy.getClass().getSimpleName()));
			Map<Hyte,List<Boolean>> codeTable = codeStrategy.getCodeTable(counts);
			System.out.print(codeTableToString(codeTable));
			
			System.out.println("  Mean symbol length");
			System.out.printf("    %.3f bits\n",
					counts.getMeanLength(codeTable));
			System.out.println("  Redundancy");
			System.out.printf("    %.3f bits\n",
					counts.getRedundancy(codeTable,Unit.BITS));
			System.out.println("Expected Compression Ratio");
			System.out.printf("    %.3f %%\n", 
					counts.getCompressionRatio(codeTable,Unit.BITS));
			
			Code<Hyte> code = new Code<Hyte>(codeTable);
			
			List<Boolean> compressed = code.encode(hytes);
			System.out.println("Compressed: "+booleanListToString(compressed));
			System.out.print("  Size: "+compressed.size()+" Bits");
			System.out.printf(" (%.3f %%)\n",
					(float)compressed.size()/(input.length*8));
			
			restored = code.decode(compressed);
		
			System.out.print("\n\n");
			
		}
		
		
		System.out.println("Restored: "+restored);
		System.out.println("  Size: "+restored.size()*4+" Bits");
		
		byte[] output = Hyte.toByteArray(restored);
		
		String textOutput = new String(output,charset);
		System.out.println("Restored data as UTF-8 text: "+textOutput);
		System.out.println("  Size: "+input.length*8+" bits");
		System.out.println();
		
	}
	
	/**
	 * Helper method to print a boolean list like <code>0100111...</code>
	 * @param list The list to display
	 * @return A string of zeros and ones
	 */
	private static String booleanListToString(List<Boolean> list) {
		StringBuffer sb = new StringBuffer();
		for(Boolean b : list) 
			sb.append(b==Boolean.FALSE?'0':'1');
		return sb.toString();
	}
	
	/**
	 * Helper method to print a code table as created by {@link Huffman}
	 * in rows like <code>[from] -> [to]</code>
	 * @param table
	 * @return
	 */
	private static String codeTableToString(Map<Hyte,List<Boolean>> table) {
		StringBuffer sb = new StringBuffer();
		for(Hyte h : table.keySet()) 
			sb.append("     "+h+" -> "+booleanListToString(table.get(h))+"\n");
		return sb.toString();
	}
	

}
