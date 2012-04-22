package bxt.unilectures.fun.jaxb;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * Some immutable test class.
 * @author Burny
 */
@XmlAccessorType(XmlAccessType.FIELD)
public class TestImmutable {
	
	private int a;
	private int b;
	
	// Only for Reflection
	@SuppressWarnings("unused")
	private TestImmutable() {}
	
	public TestImmutable(int a, int b) {
		this.a = a;
		this.b = b;
	}
	
	public int getA() {
		return a;
	}

	public int getB() {
		return b;
	}
	
	
	
}
