package bxt.unilectures.algorithmenfuergeographischeinformationssysteme.t9;

import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Scanner;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;


import org.jooq.lambda.Seq;
import org.jooq.lambda.tuple.Tuple2;

public class T9 {
	
	private final static String ARROW = " --> ";
	private final static int PRECISION = 100000;
	
	private Map<String, Long> emissionProbabilites = new HashMap<>();
	private Map<String, Long> transitionProbabilities = new HashMap<>();
	
	private Map<String, Integer> characterKey = new HashMap<String, Integer>();
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
			System.out.println(word + ARROW + Arrays.toString(keys) + ARROW + t9.word(keys));
		});
	}

	public T9(String sampleTextFilename) {
		Collection<String> tokens = readTokens(sampleTextFilename);
		
		Map<String, Long> letterCounts = tokens.stream()
				.collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
		
		Map<String, Long> digaramCounts = Seq.seq(tokens)
				.duplicate()
				.map1(s -> Seq.concat(Seq.of(" "),s))
				.map((l,r) -> Seq.zip(l, r, String::concat))
				.collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
		
		calculateEmissions(letterCounts);
		calculateTransitions(letterCounts, digaramCounts);
	}
	
	private Collection<String> readTokens(String sampleTextFilename) {
		InputStream stream = getClass().getResourceAsStream(sampleTextFilename);
		Collection<String> tokens = null;
		try(Scanner sc = new Scanner(stream)) {
			sc.useDelimiter("[0-9]*");
			tokens = Seq.seq(sc).collect(Collectors.toList());
		}
		return tokens;
	}
	
	private void calculateTransitions(Map<String, Long> letterCounts, Map<String, Long> digaramCounts) {
		digaramCounts.forEach((digram, count) -> {
			transitionProbabilities.put(digram, log((double)count/letterCounts.get(digram.substring(0, 1))));
		});
	}

	private void calculateEmissions(Map<String, Long> letterCounts) {
		Seq.concat(Seq.of(0),IntStream.range(2, 10).boxed())
		.map(this::lettersFor)
		.map(s -> s.collect(Collectors.toList()))
		.forEach(letters -> {
			long total = letters
				.stream()
				.map(letterCounts::get)
				.reduce(Long::sum)
				.get();
			letters.stream().forEach((letter) -> {
				emissionProbabilites.put(letter, log((double)letterCounts.get(letter)/total));
			});
		});
	}

	public int[] keypresses(String word) {
		return word
				.chars()
				.map(i -> characterKey.get(String.valueOf((char)i)))
				.toArray();
	}
	
	public String word(int[] keypresses) {
		Collection<Node> prev = Seq.foldLeft(
				Arrays.stream(keypresses).boxed(),
				Collections.singletonList(new Node(" ", null, 0)),
				(nodes, key) -> lettersFor(key)
					.map(letter -> maxTransition(nodes, letter))
					.collect(Collectors.toList()));
		
		Node m = maxTransition(prev, " ");
		
		return Seq
				.iterate(m, Node::getPredecessor)
				.limitUntil(Objects::isNull)
				.map(Node::getLetter)
				.reverse()
				.collect(Collectors.joining());
	}
	
	private Node maxTransition(Collection<Node> prev, String letter) {
		return Seq
				.zip(prev.stream(), prev.stream().map(
					n -> n.getProbability() + transitionProbabilities.getOrDefault(n.getLetter() + letter, 0L) + emissionProbabilites.get(letter)))
				.maxBy(Tuple2::v2).get()
				.map((predecessor, probability) -> new Node(letter, predecessor, probability));
	}
	
	private Stream<String> lettersFor(int key) {
		return characterKey
				.entrySet()
				.stream()
				.filter(e -> e.getValue() == key)
				.map(Entry::getKey);
	}
	
	private static long log(double probability) {
		 return Math.round(PRECISION*Math.log(probability));
	}
	
	private static class Node {
		private String letter;
		private Node predecessor;
		private long probability;
		
		public Node(String letter, Node predecessor, long probability) {
			this.letter = letter;
			this.predecessor = predecessor;
			this.probability = probability;
		}

		public String getLetter() {
			return letter;
		}

		public Node getPredecessor() {
			return predecessor;
		}

		public long getProbability() {
			return probability;
		}
	}
	
}
