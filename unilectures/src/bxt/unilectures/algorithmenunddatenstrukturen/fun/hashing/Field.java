package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

public class Field<T> {
	private Object[] a;
	
	public Field(int s) {
	    a = new Object[s];
	}
	
	@SuppressWarnings({"unchecked"})
	T get(int i) {
	    return (T) a[i];
	}

	void set(int i, T t) {
	    a[i]=t;
	}

	public int size() {
		return a.length;
	}

}
