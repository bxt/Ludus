package bxt.video.scanlines;

public class Buffer {
	
	private final int arraySize;
	private final int ringSize;
	private int[] data;
	private int position = 0;
	
	public Buffer(int arraySize, int ringSize) {
		this.ringSize = ringSize;
		this.arraySize = arraySize;
		clear();
	}
	
	public void get(int[] dest, int destPos) {
		System.arraycopy(data, position * arraySize, dest, destPos, arraySize);
	}
	
	public int get(int i) {
		return data[position * arraySize + i];
	}
	
	public void put(int[] source, int sourcePos) {
		System.arraycopy(source, sourcePos, data, position * arraySize, arraySize);
	}
	
	public void put(int i, int entry) {
		data[position * arraySize + i] = entry;
	}
	
	public void next() {
		position = (position + 1) % ringSize;		
	}
	
	private void clear() {
		data = new int[arraySize*ringSize];
	}	
}
