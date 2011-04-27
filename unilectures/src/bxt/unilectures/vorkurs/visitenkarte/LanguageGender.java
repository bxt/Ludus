package bxt.unilectures.vorkurs.visitenkarte;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface LanguageGender {
	Gender value();
	enum Gender {MALE,FEMALE,NEUTRUM,OTHER}
}
