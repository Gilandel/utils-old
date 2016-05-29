/*
 * #%L
 * utils-asserts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons.asserts;

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
 * But the developer can also specified a specific exception since Java 8. For
 * example:
 *
 * <pre>
 * AssertUtils.isNotNull(clazz, &quot;The class must not be null&quot;);
 * AssertUtils.isGT(i, 0, &quot;The value must be greater than zero&quot;);
 * AssertUtils.isTrue(bool, new MyException(&quot;The value must be true&quot;));
 * </pre>
 *
 * <p>
 * Mainly for internal use within the framework; consider
 * <a href="http://commons.apache.org/proper/commons-lang/">Apache's Commons
 * Lang</a> for a more comprehensive suite of {@code String} utilities.
 * 
 * <p>
 * Optionally, the checked parameters can be displayed in exception messages by
 * using %p or %1$p
 * </p>
 * 
 * <pre>
 * AssertUtils.isGT(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
 * // Exception: "The number '10' is not greater than number '20'"
 * AssertUtils.isGT(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
 * // Exception: "'20' &gt; '10'"
 * </pre>
 *
 * Based on:
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
public abstract class AssertUtils extends AbstractIterableAssert {

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
        that(actual, matcher, (String) null);
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
        that(actual, matcher, null, message, arguments);
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
        that(actual, matcher, exception, null);
    }

    private static <T, E extends Throwable> void that(final T actual, final Matcher<? super T> matcher, final E exception,
            final String message, final Object... arguments) throws E {
        if (!matcher.matches(actual)) {
            Description description = new StringDescription();
            description.appendText("Expected: ");
            description.appendDescriptionOf(matcher).appendText("\n     but: ");
            matcher.describeMismatch(actual, description);

            manageExceptions(description.toString(), exception, message, new Object[] {actual, matcher}, arguments);
        }
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final ConsumerAssert<Throwable> consumer, final Class<T> expectedException) {
        exception(consumer, expectedException, new RuntimeException("The expected exception never comes up"));
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param expectedMessage
     *            The expected exception message
     * @param <T>
     *            The generic expected exception type
     */
    public static <T extends Throwable> void exception(final ConsumerAssert<Throwable> consumer, final Class<T> expectedException,
            final String expectedMessage) {
        exception(consumer, expectedException, new RuntimeException("The expected exception never comes up"));
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param exception
     *            The exception to throw
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Exception provided
     */
    public static <T extends Throwable, E extends Throwable> void exception(final ConsumerAssert<Throwable> consumer,
            final Class<T> expectedException, final E exception) throws E {
        exception(consumer, expectedException, null, exception);
    }

    /**
     * Check that the consumed code raise the specified exception
     * 
     * @param consumer
     *            The consumer
     * @param expectedException
     *            The expected exception type
     * @param expectedMessage
     *            The expected exception message
     * @param exception
     *            The exception to throw
     * @param <T>
     *            The generic expected exception type
     * @param <E>
     *            The exception thrown if the expected exception isn't raised
     * @throws E
     *             Exception provided
     */
    public static <T extends Throwable, E extends Throwable> void exception(final ConsumerAssert<Throwable> consumer,
            final Class<T> expectedException, final String expectedMessage, final E exception) throws E {
        Throwable e = null;
        try {
            consumer.assertException();
        } catch (Throwable e1) {
            e = e1;
        }
        boolean exceptionDontMatch = e == null || !expectedException.isAssignableFrom(e.getClass());
        if (exceptionDontMatch && (expectedMessage == null || expectedMessage.equals(e.getMessage()))) {
            throw exception;
        }
    }
}
