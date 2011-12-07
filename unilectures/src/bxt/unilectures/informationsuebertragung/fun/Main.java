package bxt.unilectures.informationsuebertragung.fun;

import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import bxt.unilectures.informationsuebertragung.fun.Counts.Unit;

public class Main implements Stage<byte[],Tuple<Code<Hyte>,List<Boolean>>> {
	
	private static Charset charset = Charset.forName("UTF-8");

	/**
	 * Will run the Huffman compression
	 * <p>
	 * Will produce informative output while performing the following steps:
	 * <ul>
	 * <li> Convert bytes to hytes and utf-8 string
	 * <li> Count probabilities and dump some facts about them
	 * <li> Calculate a code table from probabilities
	 * <li> Show some facts about the code table
	 * <li> Encode the hytes to a compressed binary
	 * <li> Display compression ratio
	 * <li> Decode the compressed binary to hytes again 
	 * <li> Convert hytes to bytes and UTF-8 string
	 * <li> Hope everything stays the same :)
	 * </ul>
	 * @param args
	 */
	public static void main(String[] args) {
		
		byte[] input = {0x42, 0x65, 0x72, 0x6E, 0x68, 0x61, 0x72, 
				0x64, 0x20, 0x48, (byte)0xC3, (byte)0xA4, 0x75, 
				0x73, 0x73, 0x6E, 0x65, 0x72};
		
		/*
		// another testing value:
		byte[] input="aaaaaaltaaaaaaaaaaaaaaaaah!".getBytes(charset);
		*/
		
		Main me = new Main();
		
		if(Arrays.equals(input,me.tock(me.tick(input)))) {
			System.out.println("Worked!");
		} else {
			System.out.println("Broken!");
		}
		
	}
	
	
/* (non-Javadoc)
	 * @see bxt.unilectures.informationsuebertragung.fun.Stage#tick(byte[])
	 */
	@Override
	public Tuple<Code<Hyte>,List<Boolean>> tick (byte[] input) {
		
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
		
		Code<Hyte> code=null;
		List<Boolean> compressed=null;
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
			
			code = new Code<Hyte>(codeTable);
			
			compressed = code.encode(hytes);
			System.out.println("Compressed: "+booleanListToString(compressed));
			System.out.print("  Size: "+compressed.size()+" Bits");
			System.out.printf(" (%.3f %%)\n",
					(float)compressed.size()/(input.length*8));
			
			@SuppressWarnings("unused")
			List<Hyte> restoredDummy = code.decode(compressed);
		
			System.out.print("\n\n");
			
		}
		return new Tuple<Code<Hyte>, List<Boolean>>(code, compressed);
	}
	
	/* (non-Javadoc)
	 * @see bxt.unilectures.informationsuebertragung.fun.Stage#tock(bxt.unilectures.informationsuebertragung.fun.Tuple)
	 */
	@Override
	public byte[] tock (Tuple<Code<Hyte>,List<Boolean>> input) {
		
		Code<Hyte> code=input.getA();
		List<Boolean> compressed=input.getB();
		
		List<Hyte> restored = code.decode(compressed);
		
		System.out.println("Restored: "+restored);
		System.out.println("  Size: "+restored.size()*4+" Bits");
		
		byte[] output = Hyte.toByteArray(restored);
		
		String textOutput = new String(output,charset);
		System.out.println("Restored data as UTF-8 text: "+textOutput);
		System.out.println("  Size: "+output.length*8+" bits");
		System.out.println();
		
		return output;
		
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
