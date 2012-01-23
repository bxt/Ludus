package bxt.unilectures.informationsuebertragung.fun.staging;

public class CombinedStage<A,B,C> implements Stage<A,C> {
	
	private Stage<A,B> stage1;
	private Stage<B,C> stage2;
	
	public CombinedStage(Stage<A,B> stage1, Stage<B,C> stage2) {
		this.stage1=stage1;
		this.stage2=stage2;
	}

	@Override
	public C tick(A input) {
		return this.stage2.tick(this.stage1.tick(input));
	}

	@Override
	public A tock(C input) {
		return this.stage1.tock(this.stage2.tock(input));
	}

}
