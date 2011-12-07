package bxt.unilectures.informationsuebertragung.fun;

public interface Stage<T,S> {

	public abstract S tick(T input);

	public abstract T tock(S input);

}