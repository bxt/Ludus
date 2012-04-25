package bxt.unilectures.fun.jaxb;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import bxt.unilectures.fun.jaxb.list.TestImplementationA;
import bxt.unilectures.fun.jaxb.list.TestImplementationB;
import bxt.unilectures.fun.jaxb.list.TestParent;

@XmlRootElement(name="test")
public class TestRoot {
	
	private TestBean testBean;
	private TestImmutable testImmutable;
	
	@XmlElementWrapper(name="items")
	@XmlElements({
	    @XmlElement(name="a", type=TestImplementationA.class),
	    @XmlElement(name="b", type=TestImplementationB.class),
	})
	private List<TestParent> testItems = new ArrayList<TestParent>();
	
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
	
	public void addTestItem(TestParent testItem) {
		testItems.add(testItem);
	}
	
}
