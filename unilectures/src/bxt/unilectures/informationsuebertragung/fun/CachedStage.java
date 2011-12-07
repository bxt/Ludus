package bxt.unilectures.informationsuebertragung.fun;

public class CachedStage<A,B,C> implements Stage<A,B> {
	
	private Stage<A,Tuple<C,B>> stage;
	private C cache=null;
	
	public CachedStage(Stage<A,Tuple<C,B>> stage) {
		this.stage=stage;
	}
	
	@Override
	public B tick(A input) {
		Tuple<C,B> result=stage.tick(input);
		cache=result.getA();
		return result.getB();
	}

	@Override
	public A tock(B input) {
		return stage.tock(new Tuple<C,B>(cache,input));
	}

}
