package bxt.unilectures.informationsuebertragung.fun.staging;


public class VerbosityStage<A,B> implements Stage<A,B> {
	
	private static final String entering =
			"\n\n--->>>---------->>>-------\n%s\n--->>>---------->>>-------\n";
	
	private static final String leaving =
			"\n---<<<----------<<<-------\n%s\n---<<<----------<<<-------\n\n";
	
	private Stage<A,B> stage;
	private String name;
	
	public VerbosityStage(Stage<A,B> stage, String name) {
		this.stage=stage;
		this.name=name;
	}
	
	public VerbosityStage(Stage<A,B> stage) {
		this.stage=stage;
		this.name=stage.getClass().getSimpleName();
	}
	
	@Override
	public B tick(A input) {
		System.out.printf(entering,name);
		return stage.tick(input);
	}

	@Override
	public A tock(B input) {
		A result = stage.tock(input);
		System.out.printf(leaving,name);
		return result;
	}

}
