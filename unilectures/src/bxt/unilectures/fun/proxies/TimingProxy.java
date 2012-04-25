package bxt.unilectures.fun.proxies;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * Proxy class for timing method calls. 
 * @param <T> Type of proxied object
 */
public class TimingProxy<T> implements InvocationHandler {
	
	private T target;
	
	/**
	 * Wrap an object into a timer proxy. 
	 * @param target The object to be wrapped
	 * @param ìnterface The interface the procy is build for 
	 * @return The decorated object
	 */
	@SuppressWarnings("unchecked")
	public static <I,T extends I> I newInstance(T target, Class<I> ìnterface) {
		return (I) Proxy.newProxyInstance(
				target.getClass().getClassLoader(), 
				new Class[]{ìnterface}, new TimingProxy<T>(target));
	}
	
	private TimingProxy(T t) {
		this.target = t;
	}
	
	@Override
	public Object invoke(Object proxy, Method method, Object[] args)
			throws Throwable {
		
		Object result;
		long msStart = 0;
		
		try {
			msStart = System.currentTimeMillis();
			result = method.invoke(target, args);
		} catch (InvocationTargetException e) {
			throw e.getTargetException();
		} finally {
			long msDiff = (System.currentTimeMillis()-msStart);
			System.out.println(
					String.format("\n%s\n> %d ms",
							method.toGenericString(),
							msDiff));
		}
		
		return result;
		
	}

}
