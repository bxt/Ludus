package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * Toy implementation of a hash map which lets you choose between
 * different hash and probe implementations. 
 * @author Burny
 * @param <Value> Type to carry
 */
public class HashMap<Value> {
	
	/**
	 * If or not to output some more information while operating
	 */
	private boolean verbose = false;
	
	/**
	 * Used to get an initial hash value for a key
	 */
	private Hashing hasher;
	/**
	 * Used to get another hash value for a key when others are used
	 */
	private Probing prober;
	
	/**
	 * Maximum entry count in this hash table
	 */
	private int size;
	/**
	 * Current entry count
	 */
	private int count;
	/**
	 * Entries indexed by an int
	 */
	private Field<Bucket<Value>> values;
	
	/**
	 * Constructor for no verbosity
	 * @param size Max. entry count (prime strongly encouraged)
	 * @param hasher For initial hash values
	 * @param prober For additional hash values
	 */
	public HashMap(int size, Hashing hasher, Probing prober) {
		this.hasher = hasher;
		this.prober = prober;
		this.size = size;
		count = 0;
		values = new Field<Bucket<Value>>(size);
	}
	
	/**
	 * Alternate constructor for en/disabling verbosity
	 * @param size Max. entry count
	 * @param hasher For initial hash values
	 * @param prober For additional hash values
	 * @param verbose If or not to dump infos on execution
	 */
	public HashMap(int size, Hashing hasher, Probing prober, boolean verbose) {
		this(size, hasher, prober);
		this.verbose=verbose;
	}
	
	/**
	 * Get the value associated with a certain key
	 * @param key The key
	 * @return The value or <code>null</code> if nonexistent
	 */
	public Value get(int key) {
		int hash = getHash(key);
		Value returnValue = null;
		if (values.get(hash)!=null)
			returnValue = values.get(hash).getValue();
		return returnValue;
	}
	
	/**
	 * 	Associate a key with a value
	 * @param key The new items key (will overwrite if used before)
	 * @param value The new value
	 */
	public void put(int key, Value value) {
		if(count>=size) throw new RuntimeException("Full");
		int hash = getHash(key);
		if(values.get(hash)==null) count++;
		values.set(hash,new Bucket<Value>(key, value));
	}
	
	/**
	 * Remove a key's association
	 * @param key The key to delete the value from
	 * @return If or not a value was actually detached
	 */
	public boolean remove(int key) {
		int hash = getHash(key);
		boolean returnValue = values.get(hash) != null;
		values.set(hash,null);
		if(returnValue) count--;
		assert count >= 0;
		return returnValue;
	}
	
	/**
	 * Finding the next unused hash position for a key
	 * @param key The key
	 * @return The first unused position in the value list
	 */
	private int getHash(int key) {
		if(verbose) System.out.println(printHashTrace(key));
		
		int hash = hasher.getHash(key, size);
		
		for(int i=1;values.get(hash)!=null&&values.get(hash).getKey()!=key;i++)
			hash = prober.getHash(key, i, size);
		
		return hash;
	}
	
	/**
	 * Printing the unsucessful key choises
	 * @param key
	 * @return
	 */
	private String printHashTrace(int key) {
		StringBuilder sb=new StringBuilder();
		int hash = hasher.getHash(key, size);
		sb.append(String.format("try #%04d", hash));
		
		for(int i=1;values.get(hash)!=null&&values.get(hash).getKey()!=key;i++) {
			hash = prober.getHash(key, i, size);
			sb.append(String.format(", #%04d", hash));
		}
		
		sb.append('\n');
		return sb.toString();
	}
	
	/**
	 * Print the whole hash table with hash, keys, values and empty spaces
	 */
	public String toString() {
		StringBuilder sb=new StringBuilder();
		for(int i=0;i<size;i++) {
			sb.append(String.format(".%02d ",i));
			if(values.get(i)!=null) {
				sb.append(String.format(
						"#%04d: %s", 
						values.get(i).getKey(),
						values.get(i).getValue().toString()
						));
			} else {
				sb.append('*');
			}
			sb.append('\n');
		}
		return sb.toString();
	}
	
	/**
	 * An entriy with its key and value in the value list
	 * @author Burny
	 * @param <Value> Type to carry
	 */
	private static class Bucket<Value> {
		int key;
		Value value;
		public Bucket(int key, Value value) {
			super();
			this.key = key;
			this.value = value;
		}
		public int getKey() {
			return key;
		}
		public Value getValue() {
			return value;
		}
	}
	
}
