package bxt.unilectures.fun.jaxb;

import java.util.Date;

import javax.xml.bind.annotation.XmlAttribute;

public class TestBean {
	
	private Date time;
	private String name;
	private Integer amount;
	
	TestBean() {}
	
	public TestBean(Date time, String name, Integer amount) {
		this.time = time;
		this.name = name;
		this.amount = amount;
	}

	@XmlAttribute
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public Integer getAmount() {
		return amount;
	}
	public void setAmount(Integer amount) {
		this.amount = amount;
	}
	
}
