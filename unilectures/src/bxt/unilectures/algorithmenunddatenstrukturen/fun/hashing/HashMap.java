package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

public class HashMap<Value> {
	
	private final static boolean VERBOSE = true;
	
	private Hashing hasher;
	private Probing prober;
	
	private int size;
	private int count;
	private Field<Bucket<Value>> values;
	
	public HashMap(int size, Hashing hasher, Probing prober) {
		this.hasher = hasher;
		this.prober = prober;
		this.size = size;
		count = 0;
		values = new Field<Bucket<Value>>(size);
	}

	public Value get(int key) {
		int hash = getHash(key);
		Value returnValue = null;
		if (values.get(hash)!=null)
			returnValue = values.get(hash).getValue();
		return returnValue;
	}
	
	public void put(int key, Value value) {
		if(count>=size) throw new RuntimeException("Full");
		int hash = getHash(key);
		if(values.get(hash)==null) count++;
		values.set(hash,new Bucket<Value>(key, value));
	}
	
	public boolean remove(int key) {
		int hash = getHash(key);
		boolean returnValue = values.get(hash) != null;
		values.set(hash,null);
		if(returnValue) count--;
		assert count >= 0;
		return returnValue;
	}
	
	
	private int getHash(int key) {
		if(VERBOSE) System.out.println(printHashTrace(key));
		
		int hash = hasher.getHash(key, size);
		
		for(int i=1;values.get(hash)!=null&&values.get(hash).getKey()!=key;i++)
			hash = prober.getHash(key, i, size);
		
		return hash;
	}
	
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
