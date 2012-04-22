package bxt.unilectures.fun.jaxb;

import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name="test")
public class TestRoot {
	
	private TestBean testBean;
	private TestImmutable testImmutable;
	
	public TestBean getTestBean() {
		return testBean;
	}
	public void setTestBean(TestBean testBean) {
		this.testBean = testBean;
	}
	public TestImmutable getTestImmutable() {
		return testImmutable;
	}
	public void setTestImmutable(TestImmutable testImmutable) {
		this.testImmutable = testImmutable;
	}
	
}
