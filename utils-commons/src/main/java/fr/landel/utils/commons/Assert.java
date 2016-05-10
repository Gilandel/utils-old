/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.StringDescription;

/**
 * Assertion utility class that assists in validating arguments.
 *
 * <p>
 * Useful for identifying programmer errors early and clearly at runtime.
 *
 * <p>
 * For example, if the contract of a public method states it does not allow
 * {@code null} arguments, {@code Assert} can be used to validate that contract.
 * Doing this clearly indicates a contract violation when it occurs and protects
 * the class's invariants.
 *
 * <p>
 * Typically used to validate method arguments rather than configuration
 * properties, to check for cases that are usually programmer errors rather than
 * configuration errors. In contrast to configuration initialization code, there
 * is usually no point in falling back to defaults in such methods.
 *
 * <p>
 * This class is similar to JUnit's assertion library. If an argument value is
 * deemed invalid, an {@link IllegalArgumentException} is thrown (typically).
 * For example:
 *
 * <pre>
 * Assert.notNull(clazz, &quot;The class must not be null&quot;);
 * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 * 
 * <p>
 * To display parameters in exception messages, use %p or %1$p
 * </p>
 * 
 * <pre>
 * Assert.isGT(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * Assert.isGT(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
 *
 * @see <a href=
 *      "http://docs.spring.io/spring/docs/2.0.x/api/org/springframework/util/Assert.html?is-external=true">
 *      org.springframework.util.Assert</a>
 *
 * @author Keith Donald
 * @author Juergen Hoeller
 * @author Colin Sampaleanu
 * @author Rob Harrop
 * @author Sam Brannen
 * @author Gilles Landel
 * @since 1.1.2
 */
public abstract class Assert extends AbstractNumberAssert {

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array) {
        hasNoNullElements(array, (String) null);
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array, &quot;The array must have non-null elements&quot;);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object array contains a {@code null} element
     */
    public static void hasNoNullElements(Object[] array, final String message, final Object... arguments) {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    throw new IllegalArgumentException(
                            getMessage("this array must not contain any null elements", message, new Object[] {array}, arguments));
                }
            }
        }
    }

    /**
     * Assert that an array has no null elements. Note: Does not complain if the
     * array is empty!
     * 
     * <pre>
     * Assert.hasNoNullElements(array, exceptionToThrowOnError);
     * </pre>
     * 
     * @param array
     *            the array to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if object array contains a {@code null} element. The standard
     *             exception is appended as suppressed.
     */
    public static <E extends Throwable> void hasNoNullElements(Object[] array, final E exception) throws E {
        if (array != null) {
            for (Object element : array) {
                if (element == null) {
                    exception.addSuppressed(new IllegalArgumentException("this array must not contain any null elements"));
                    throw exception;
                }
            }
        }
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param <T>
     *            the type of the object to check
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public static <T> void that(final T actual, final Matcher<? super T> matcher) {
        Assert.that(actual, matcher, (String) null);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param message
     *            a message, if {@code null} use the default assertion message
     *            (%p or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <T>
     *            the type of the object to check
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public static <T> void that(final T actual, final Matcher<? super T> matcher, final String message, final Object... arguments) {
        if (!matcher.matches(actual)) {
            Description description = new StringDescription();
            description.appendText(getMessage("", message, new Object[] {actual, matcher}, arguments));
            description.appendText("\nExpected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            throw new IllegalArgumentException(description.toString());
        }
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param actual
     *            an object to check
     * @param matcher
     *            an hamcrest matcher
     * @param exception
     *            the exception to throw on error
     * @param <T>
     *            the type of the object to check
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     */
    public static <T, E extends Throwable> void that(final T actual, final Matcher<? super T> matcher, final E exception) throws E {
        if (!matcher.matches(actual)) {
            Description description = new StringDescription();
            description.appendText("Expected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            exception.addSuppressed(new IllegalArgumentException(description.toString()));
            throw exception;
        }
    }
}