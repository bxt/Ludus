package bxt.unilectures.fun.bloomfilter;

public class DerivedHashFunction implements HashFunction<Object> {

	int factor;
	
	public DerivedHashFunction(int factor) {
		super();
		this.factor = factor;
	}

	@Override
	public int hashCode(Object t) {
		return t == null ? 0 : t.hashCode() * factor;
	}

	@Override
	public String toString() {
		return "DerivedHashFunction [factor=" + factor + "]";
	}
	
}
