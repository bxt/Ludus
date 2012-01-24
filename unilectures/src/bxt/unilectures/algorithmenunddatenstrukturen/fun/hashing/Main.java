package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		/*
		Hashing hasher = new DivisionHashing();
		Probing prober = new LinearProbing();
		*/
		Hashing hasher = new MultiplicationHashing();
		Probing prober = new DoubleProbing(new MultiplicationHashing(1.1f));
		
		HashMap<String> hashMap = new HashMap<>(5, hasher, prober);
		
		hashMap.put(3, "Hallo ");
		System.out.println(hashMap);
		
		hashMap.put(8, "Welt!");
		System.out.println(hashMap);
		
		hashMap.put(13, "Java");
		System.out.println(hashMap);
		
		hashMap.put(8, "Oracle");
		System.out.println(hashMap);
		
		hashMap.put(1, "Oh");
		System.out.println(hashMap);
		
		hashMap.put(2, "No");
		System.out.println(hashMap);
		
		System.out.println(hashMap.remove(8));
		System.out.println(hashMap);
		
		hashMap.put(4, "So op!");
		System.out.println(hashMap);
		
		try {
			hashMap.put(4, "Mah!");
			System.out.println(hashMap);
		} catch (Exception e) {
			System.out.println("Causes: "+e);
		}
		
	}

}
