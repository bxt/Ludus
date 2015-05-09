package bxt.unilectures.algorithmenfuergeographischeinformationssysteme.t9;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.function.Function;
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
			for (String curr : (Iterable<String>) () -> sc) {
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
		
		Collection<Node> prev = Arrays.stream(keypresses).boxed().reduce(
				Collections.singletonList(new Node(" ", null, 0)),
				(nodes, key) -> lettersFor(key).map(letter -> maxTransition(nodes, letter)).collect(Collectors.toList()),
				(a,b) -> {throw new UnsupportedOperationException();});
		
		Node m = maxTransition(prev, " ");
		
		String result = "";
		while (m.predecessor != null) {
			result = m.letter + result;
			m = m.predecessor;
		}
		
		return result;
	}
	
	private Node maxTransition(Collection<Node> prev, String letter) {
		Function<Node, Integer> f = n -> n.probability + digaramCounts.getOrDefault(n.letter + letter, 0);
		Node predecessor = prev.stream().max(Comparator.comparing(f)).get();
		return new Node(letter, predecessor, f.apply(predecessor));
	}
	
	private Stream<String> lettersFor(int key) {
		return characterKey.entrySet().stream().filter(e -> e.getValue() == key).map(Entry::getKey);
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
