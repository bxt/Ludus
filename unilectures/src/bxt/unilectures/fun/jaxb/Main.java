package bxt.unilectures.fun.jaxb;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Date;

import javax.xml.bind.JAXB;

import bxt.unilectures.fun.jaxb.list.TestImplementationA;
import bxt.unilectures.fun.jaxb.list.TestImplementationB;

public class Main {

	private static final String HALLO_WELT = "Hallo, Welt. ";

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		TestRoot testRoot = new TestRoot();
		TestBean testBean = new TestBean();
		testBean.setAmount(12);
		testBean.setTime(new Date());
		testBean.setName(HALLO_WELT);
		testRoot.setTestBean(testBean);
		testRoot.setTestImmutable(new TestImmutable(2,3));
		
		TestImplementationA testItemA = new TestImplementationA();
		testItemA.setValue("A value. ");
		testRoot.addTestItem(testItemA);
		
		TestImplementationB testItemB = new TestImplementationB();
		testItemB.setAmount(100);
		testRoot.addTestItem(testItemB);
		
		JAXB.marshal(testRoot, System.out);
		
		
		ByteArrayOutputStream xmlOut = new ByteArrayOutputStream();
		JAXB.marshal(testRoot, xmlOut);
		InputStream xmlIn = new ByteArrayInputStream(xmlOut.toByteArray());
		TestRoot testRoot2 = JAXB.unmarshal(xmlIn, TestRoot.class);
		JAXB.marshal(testRoot2, System.out);
		
	}

}
