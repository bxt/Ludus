package bxt.unilectures.algorithmenfuergeographischeinformationssysteme.t9;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.stream.Collectors;
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
		Stream.of("hello", "world", "the", "in", "and", "reuter", "bernhard", "people", "reliability", "many", "kind", "words", "for", "you").forEach(word -> {
			int[] keys = t9.keypresses(word);
			System.out.print(word);
			System.out.print(Arrays.toString(keys));
			System.out.print(t9.word(keys));
			System.out.println();
		});
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
					digaramCounts.put(digram, digaramCounts.getOrDefault(digram, 0) + 1);
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
		
		Node dummy = new Node(" ", null, 0);
		List<Node> prev = new ArrayList<T9.Node>();
		prev.add(dummy);
		
		for(int i = 0; i < keypresses.length; i++) {
			List<Node> curr = new ArrayList<T9.Node>();
			for (String letter : candidatesFor(keypresses[i])) {
				Node pre = null;
				int max = 0;
				for(Node n : prev) {
					int foo = n.probability + digaramCounts.getOrDefault(n.letter + letter, 0);
					if (foo > max) {
						max = foo;
						pre = n;
					}
				}
				Node m = new Node(letter, pre, max);
				curr.add(m);
			}
			prev = curr;
		}
		
		Node m;
		{
			Node pre = null;
			int max = 0;
			for(Node n : prev) {
				int foo = n.probability + digaramCounts.getOrDefault(n.letter + " ", 0);
				if (foo > max) {
					max = foo;
					pre = n;
				}
			}
			m = new Node("", pre, max);
		}
		
		String result = "";
		while (m.predecessor != null) {
			result = m.letter + result;
			m = m.predecessor;
		}
		
		return result;
	}
	
	private List<String> candidatesFor(int key) {
		return characterKey.entrySet().stream().filter(e -> e.getValue() == key).map(Entry::getKey).collect(Collectors.toList());
	}
	
	private static class Node {
		private String letter;
		private Node predecessor;
		private int probability;
		
		public Node(String letter, Node predecessor, int probability) {
			this.letter = letter;
			this.predecessor = predecessor;
			this.probability = probability;
		}
	}
	
}
