package bxt.unilectures.vorkurs.fun;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class Javascript {
	
	/**
	 * The function name we will call
	 */
	private final static String main="main";
	
	/**
	 * The script we want to execute
	 */
	private final static String script=""+
	"println('Creating functions...');"+
	"function hW(whom) {"+
	"	return function(){"+
	"		println('Hello, '+whom+'!');"+
	"	}"+
	"}"+
	"function "+main+"(whom) {"+
	"	hW(whom)();"+
	"}";
	
	/**
	 * Run a hello world through JS scripting
	 * @param args Command-line args (unused)
	 * @see <a href="http://java.sun.com/developer/technicalArticles/J2SE/Desktop/scripting/">John O'Conner: Scripting for the Java Platform</a> 
	 */
	public static void main(String[] args) {
		ScriptEngineManager mgr = new ScriptEngineManager();
		System.out.println("Installed engines:");
		for(ScriptEngineFactory f : mgr.getEngineFactories()) {
			System.out.println(f.getEngineName());
		}
		ScriptEngine jsEngine = mgr.getEngineByName("JavaScript");
		try {
			jsEngine.eval(script);
			Invocable invocableEngine = (Invocable) jsEngine;
			try {
				System.out.println("Output from script:");
				invocableEngine.invokeFunction(main,"World");
			} catch (NoSuchMethodException e) {
				System.out.println("Tried to execute script without "+
						main+"() function.");
				e.printStackTrace();
			}
		} catch (ScriptException e) {
			e.printStackTrace();
		}
	}

}
