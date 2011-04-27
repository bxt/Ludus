package bxt.unilectures.vorkurs.visitenkarte;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CliPrompt {
	String value();
}
