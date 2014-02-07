package bxt.unilectures.fun.bloomfilter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class App {

	public static void main(String[] args) {
		int size = 100;
		
		Collection<String> testInputs = Arrays.asList(new String[]
				{ "foo", "bar", "baz", "bam", "42", "1337", "3.14", "0815", "911"
				, "19111990", "foobar", "fruchtbar", "und", "vermehret euch!"});
		
		BloomFilter<String> bloomFilter = new BloomFilter<String>(size, hashFunctionsFor(new int[]{3,5}));
		bloomFilter.addAll(testInputs);
		
		BloomFilter<String> hashSet = new BloomFilter<String>(size, hashFunctionsFor(new int[]{1}));
		hashSet.addAll(testInputs);
		
		System.out.println();
		System.out.println("            | BLOOM | HASHS | ACTUAL");
		System.out.println(" -------------------------------------");
		for (String string : new String[]{"foo", "the answer", "Bernhard", "bernhard"})
			System.out.println(String.format
					( "%11s | %-5b | %-5b | %-5b"
					, string
					, bloomFilter.contains(string)
					, hashSet.contains(string)
					, testInputs.contains(string)
					));
	}
	
	private static <T> Collection<HashFunction<? super String>> hashFunctionsFor(int[] factors) {
		Collection<HashFunction<? super String>> hashFunctions = new ArrayList<>();
		for (int factor : factors) {
			hashFunctions.add(new DerivedHashFunction(factor));
		}
		return hashFunctions;
	}

}
