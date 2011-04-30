package bxt.unilectures.vorkurs.visitenkarte;

import java.io.InputStream;
import java.io.PrintStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Scanner;

/**
 * Build a new Instance and accumulate with values from CLI user input
 * @author bxt
 *
 */
public class CliInputFactory {
	/**
	 * A default prompt message
	 */
	private static final String defaultPromt="Bitte geben sie ein(e) %s ein: ";
	/**
	 * Read user input from here
	 * @see System.in
	 */
    private InputStream in=null;
    /**
     * Write prompts and errors into this stream
     * @see System.out
     */
    private PrintStream out=null;
    /**
     * No-arg constructor, defaulting to System's streams
     */
	CliInputFactory () {
		this(System.in,System.out);
	}
	/**
	 * Use custom streams
	 * @param in Read user input from here
	 * @param out Write prompts and errors into this stream
	 */
	CliInputFactory(InputStream in, PrintStream out) {
		this.in=in;
		this.out=out;
	}
	/**
	 * Create a class using CLI input
	 * 
	 * Like {@link #create(Class, String)} but with default message {@link #defaultPromt}
	 * @param <T> Generic target type of the desired instance
	 * @param type Reflection target type of the desired instance
	 * @return The resulting instance
	 */
	public <T> T create(Class<T> type) {
		return create(type,defaultPromt);
	}
	/**
	 * Create a class using CLI input and a prompt message
	 * @param <T> Generic target type of the desired instance
	 * @param type Reflection target type of the desired instance
	 * @param message A message to display as prompt. One %s will be replaced with the simple name of the target type
	 * @return The resulting instance
	 */
	public <T> T create(Class<T> type,String message) {
		out.println();
		if(type.getAnnotation(CliPrompt.class) != null) {
			message=type.getAnnotation(CliPrompt.class).value();
		}
		out.printf(message,type.getSimpleName());
		try {
			if (type.isAssignableFrom(String.class)) {
				Scanner sc = new Scanner(in);
				String created= sc.nextLine();
				return type.cast(created);
			} else if (type.isAssignableFrom(Integer.class)) {
				Scanner sc = new Scanner(in);
				Integer created= sc.nextInt();
				return type.cast(created);
			} else if (type.isAssignableFrom(Double.class)) {
				Scanner sc = new Scanner(in);
				Double created= sc.nextDouble();
				return type.cast(created);
			} else {
				T created = type.newInstance();
				createChildren(created);
				return created;
			}
		} catch (InstantiationException e) {
			e.printStackTrace(out);
		} catch (IllegalAccessException e) {
			e.printStackTrace(out);
		}
		return null;
	}
	/**
	 * For all setters display prompts too
	 * @param <T> generic target type
	 * @param object target object whose setters are iterated
	 */
	private <T> void createChildren(T object) {
		Method[] methods = object.getClass().getMethods();
		for(Method method : methods){
			String methodName = method.getName();
			String name=methodName.substring(3);
			boolean hasSetterName = methodName.startsWith("set");
			boolean hasOneAndOnlyOneArgument = 1 == method.getParameterTypes().length;
			if(hasSetterName && hasOneAndOnlyOneArgument) {
				Object[] params={create(method.getParameterTypes()[0],"Bitte "+name+" (%s) eingeben: ")};
				try {
					method.invoke(object, params);
				} catch (IllegalArgumentException e) {
					e.printStackTrace(out);
				} catch (IllegalAccessException e) {
					e.printStackTrace(out);
				} catch (InvocationTargetException e) {
					e.printStackTrace(out);
				}
			}
		}
	}
}
