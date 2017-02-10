package fr.landel.utils.commons.builder;

import java.util.function.Function;

/**
 * 
 * @since Nov 15, 2016
 * @author Gilles
 *
 */
public class HashCodeBuilder extends org.apache.commons.lang3.builder.HashCodeBuilder{
	
	/**
	 * Append the {@code hashCode} returned by the {@code getter} function. The {@code getter} method is only applied if the {@link Object} are not {@code null}.
	 * 
	 * @param object the first object
	 * @param getter the function to apply if both objects are not {@code null}
	 * @param <T> the check object type
	 * @param <X> the sub type
	 * @return the current builder
	 */
	public <T, X> HashCodeBuilder append(final T object, final Function<T, X> getter) {
		if (object !=null) {
			this.append(getter.apply(object));
		}
		return this;
	}
}
