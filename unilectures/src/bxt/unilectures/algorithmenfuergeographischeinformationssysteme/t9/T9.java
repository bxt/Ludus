package bxt.unilectures.algorithmenfuergeographischeinformationssysteme.t9;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Stream;

public class T9 {
	
	Map<String, Integer> digaramCounts = new HashMap<String, Integer>();
	Map<String, Integer> characterKey = new HashMap<String, Integer>();
	{
		Stream.of("a","b","c"    ).forEach(c -> characterKey.put(c, 2));
		Stream.of("d","e","f"    ).forEach(c -> characterKey.put(c, 3));
		Stream.of("g","h","i"    ).forEach(c -> characterKey.put(c, 4));
		Stream.of("j","k","l"    ).forEach(c -> characterKey.put(c, 5));
		Stream.of("m","n","o"    ).forEach(c -> characterKey.put(c, 6));
		Stream.of("p","q","r","s").forEach(c -> characterKey.put(c, 7));
		Stream.of("t","u","v"    ).forEach(c -> characterKey.put(c, 8));
		Stream.of("w","x","y","z").forEach(c -> characterKey.put(c, 9));
		Stream.of(" "            ).forEach(c -> characterKey.put(c, 0));
	}
	
	public static void main(String[] args) {
		T9 t9 = new T9("sample.txt");
		int[] keys = t9.keypresses("bernhard");
		System.out.println(Arrays.toString(keys));
		System.out.println(t9.word(keys));
	}

	public T9(String sampleTextFilename) {
		InputStream stream = getClass().getResourceAsStream(sampleTextFilename);
		try(Scanner sc = new Scanner(stream)) {
			sc.useDelimiter("");
			String prev = null;
			while(sc.hasNext()) {
				String curr = sc.next();
				if(prev != null) {
					String digram = prev + curr;
					if(!digaramCounts.containsKey(digram))
						digaramCounts.put(digram, 1);
					else
						digaramCounts.put(digram, digaramCounts.get(digram) + 1);
				}
				sc.skip("[0-9]*");
				prev = curr;
			}
		}
		System.out.println(digaramCounts);
	}
	
	public int[] keypresses(String word) {
		return word.chars().map(i -> characterKey.get(String.valueOf((char)i))).toArray();
	}
	
	public String word(int[] keypresses) {
		return "hello"; // TODO
	}
	
}
