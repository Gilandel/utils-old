package fr.landel.utils.commons.builder;

import java.util.function.Function;

/**
 * {@inheritDoc}
 * 
 * @since Nov 15, 2016
 * @author Gilles
 *
 */
public class EqualsBuilder extends org.apache.commons.lang3.builder.EqualsBuilder {

	/**
	 * Test if two {@link Object} returned by the {@code getter} function are
	 * equal using their {@code equals} method. The {@code getter} method is
	 * only applied if both {@link Object} are not {@code null}.
	 * 
	 * @param lhs
	 *            the first object
	 * @param rhs
	 *            the second object
	 * @param getter
	 *            the function to apply if both objects are not {@code null}
	 * @param <T>
	 *            the check object type
	 * @param <X>
	 *            the sub type
	 * @return the current builder
	 */
	public <T, X> EqualsBuilder append(final T lhs, final T rhs, final Function<T, X> getter) {
		if (lhs != null && rhs != null) {
			this.append(getter.apply(lhs), getter.apply(rhs));
		} else {
			this.setEquals(lhs == null && rhs == null);
		}
		return this;
	}
}
