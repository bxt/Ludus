package bxt.unilectures.informationsuebertragung.fun;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Blockify implements Stage<List<Boolean>,List<List<Boolean>>> {

	private int blocksize=32;
	
	public Blockify() {
		
	}
	
	public Blockify(int blocksize) {
		this.blocksize=blocksize;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		List<Boolean> input=Arrays.asList(new Boolean[]{
				Boolean.TRUE,Boolean.FALSE,Boolean.TRUE,Boolean.FALSE,
				Boolean.TRUE,Boolean.TRUE,Boolean.FALSE,Boolean.FALSE,
				Boolean.FALSE,Boolean.FALSE,Boolean.TRUE,Boolean.TRUE
		});
		
		Blockify me = new Blockify(4);
		
		if((input.equals(me.tock(me.tick(input))))) {
			System.out.println("Worked!");
		} else {
			System.out.println("Broken!");
		}
	}

	@Override
	public List<List<Boolean>> tick(List<Boolean> input) {
		
		input=new ArrayList<Boolean>(input); // copy
		
		System.out.println("Not yet blockified:");
		System.out.println(booleanListToString(input));
		
		List<List<Boolean>> blocks = new ArrayList<List<Boolean>>();
		input.add(Boolean.TRUE); // a stop one
		
		// copy blocks
		int i=blocksize;
		for(;i<input.size();i+=blocksize) {
			blocks.add(input.subList(i-blocksize,i));
		}
		
		// fill last block with zeros
		i-=blocksize;
		List<Boolean> lastBlock =
				new ArrayList<Boolean>(input.subList(i,input.size()));
		for(int k=blocksize-lastBlock.size();k>0;k--)
			lastBlock.add(Boolean.FALSE);
		blocks.add(lastBlock);
		
		System.out.printf("Blockified %d bits to %d blocks á %d bits:\n", 
				input.size()-1,blocks.size(),blocksize);
		System.out.println(booleanBlocksToString(blocks));
		
		System.out.print("\n\n");
		
		return blocks;
	}

	@Override
	public List<Boolean> tock(List<List<Boolean>> input) {
		
		System.out.println("Some blocks of bits:");
		System.out.println(booleanBlocksToString(input));
		
		// merge all blocks
		List<Boolean> stream= new ArrayList<Boolean>();
		for(List<Boolean> block : input) {
			stream.addAll(block);
		}
		
		// remove tail zeros
		while(stream.get(stream.size()-1)==Boolean.FALSE)
			stream.remove(stream.size()-1);
		
		stream.remove(stream.size()-1); // remove final one
		
		System.out.println("Again unblockified:");
		System.out.println(booleanListToString(stream));
		
		return stream;
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
