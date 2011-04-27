package bxt.unilectures.vorkurs.visitenkarte;

import java.lang.annotation.*;

/**
 * Set a custom prompt message used by {@link CliInputFactory}
 * @author bxt
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CliPrompt {
	/**
	 * Custom prompt message, first %s is replaced with target type
	 * @return Custom prompt message string
	 */
	String value();
}
