package bxt.unilectures.vorkurs.fun;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;

public class LuckyNumbers {
	public static int[] generateLuckys(int endNumber) {
		endNumber++;
		int startNumber=1;
		int startEvery=2;
		BitSet luckyBools=new BitSet(endNumber);
		luckyBools.set(startNumber,endNumber);
		int every=startEvery;
		for(int iteration=0;;iteration++) {
			int count=0;
			int hits=0;
			int newEvery=0;
			for(int i=startNumber;i<endNumber;i++) {
				while(!luckyBools.get(i)&&i<endNumber) i++;
				count++;
				if(count%every==0) {
					luckyBools.set(i, false);
				} else {
					hits++;
					if(hits==iteration+startEvery) newEvery=i;
				}
			}
			every=newEvery;
			if(every>count) break;
		}
		return bitSetToArray(luckyBools,endNumber);
	}
	public static int[] bitSetToArray(BitSet bitSet, int limit) {
		List<Integer> intList=new ArrayList<Integer>(limit);
		for(int i=0;i<limit;i++) {
			if(bitSet.get(i)) intList.add(i);
		}
		return ArrayUtils.toPrimitive(intList.toArray(new Integer[0]));
	}
	public static void print(int[] ns) {
		for(int i=0;i<ns.length;i++) {
			System.out.print(ns[i]+" ");
		}
		System.out.println();
	}
	public static void main(String[] args) {
		int[] luckys=generateLuckys(303);
		print(luckys);
	}
}
