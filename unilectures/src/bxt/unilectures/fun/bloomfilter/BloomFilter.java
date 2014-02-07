package bxt.unilectures.fun.bloomfilter;

import java.util.BitSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

public class BloomFilter<E> implements Set<E> {
	
	private Collection<HashFunction<? super E>> hashFunctions;
	private BitSet bitSet;
	private int size;
	private int count = 0;
	
	public BloomFilter(int size, Collection<HashFunction<? super E>> hashFunctions) {
		this.hashFunctions = hashFunctions;
		this.size = size;
		bitSet = new BitSet(size);
	}
	
	@Override
	public boolean add(E element) {
		count++;
		boolean contained = true; 
		for (int hashCode : calculateHashCodes(element)) {
			contained &= bitSet.get(hashCode);
			bitSet.set(hashCode);
		}
		return contained;
	}
	
	@Override
	public boolean addAll(Collection<? extends E> collection) {
		boolean changed = false;
		for (E e : collection) changed |= add(e);
		return changed;
	}
	
	@Override
	public void clear() {
		count = 0;
		bitSet.clear();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public boolean contains(Object object) {
		boolean contained = true;
		E element = (E) object;
		for (int hashCode : calculateHashCodes(element)) {
			contained &= bitSet.get(hashCode);
		}
		return contained;
	}
	
	@Override
	public boolean containsAll(Collection<?> collection) {
		boolean containsAll = true;
		for (Object o : collection) containsAll &= contains(o);
		return containsAll;
	}
	
	@Override
	public boolean isEmpty() {
		return size() == 0;
	}
	
	@Override
	public int size() {
		return count; 
	}
	
	@Override
	public Iterator<E> iterator() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public boolean remove(Object element) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public boolean removeAll(Collection<?> collection) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public boolean retainAll(Collection<?> collection) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public Object[] toArray() {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public <T> T[] toArray(T[] array) {
		throw new UnsupportedOperationException();
	}
	
	protected int[] calculateHashCodes(E element) {
		int[] hashCodes = new int[hashFunctions.size()];
		int i = 0;
		for (HashFunction<? super E> hashFunction : hashFunctions) {
			int hashCode = hashFunction.hashCode(element) % size;
			if (hashCode<0) hashCode += size;
			hashCodes[i++] = hashCode;
		}
		return hashCodes;
	}

	@Override
	public String toString() {
		return "BloomFilter [count=" + count + ", size=" + size
				+ ", hashFunctions.size()=" + hashFunctions.size()
				+ ", hashFunctions=" + hashFunctions + ", bitSet=" + bitSet
				+ "]";
	}
}
