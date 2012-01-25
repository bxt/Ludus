package bxt.unilectures.algorithmenunddatenstrukturen.fun.hashing;

/**
 * An array using generics
 * @author Burny
 * @param <T> Dynamic content type
 */
public class Field<T> {
	/**
	 * Actual array, only type safe by guaranteeing secure insets
	 */
	private Object[] a;
	
	/**
	 * Build a new array with an initial capacity
	 * @param s Number of elements to store at most
	 */
	public Field(int s) {
	    a = new Object[s];
	}
	
	/**
	 * Read an element with an index
	 * @param i The index
	 * @return The desired element or null if not found
	 */
	@SuppressWarnings({"unchecked"})
	T get(int i) {
	    return (T) a[i];
	}
	
	/**
	 * Insert an element at a position, overwriting
	 * @param i The target index
	 * @param t The new element
	 */
	void set(int i, T t) {
	    a[i]=t;
	}
	
	/**
	 * How many items this field will store at most
	 * @return Capacity of this array
	 */
	public int size() {
		return a.length;
	}

}
