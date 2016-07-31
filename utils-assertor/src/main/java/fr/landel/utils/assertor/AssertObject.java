/*
 * #%L
 * utils-assertor
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.assertor;

import java.util.Locale;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.StringDescription;

import fr.landel.utils.commons.CastGenerics;
import fr.landel.utils.commons.ClassUtils;
import fr.landel.utils.commons.function.PredicateThrowable;

/**
 * Assertion utility class that assists in validating arguments for objects.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <A>
 *            the assert class type
 * @param <T>
 *            The object type
 */
public class AssertObject<A extends AssertObject<A, T>, T> extends AbstractAssertObject<A, T> {

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     * @param type
     *            The param type
     */
    protected AssertObject(final T object, final int type) {
        super(object, type);
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNull().toThrow(anException);
     * </pre>
     * 
     * @return The operator
     */
    public Operator<A, T> isNull() {
        return this.isNull(new StringBuilder("the object argument '").append(this.getParam()).append("' must be null"));
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNull().toThrow(anException);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<A, T> isNull(final CharSequence message, final Object... arguments) {
        return this.isNull((Locale) null, message, arguments);
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNull().toThrow(anException);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<A, T> isNull(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> this.get() == null, null, message, arguments, locale);
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNotNull().toThrow(anException);
     * </pre>
     * 
     * @return The operator
     */
    public Operator<A, T> isNotNull() {
        return this.isNotNull(new StringBuilder("the object argument '").append(this.getParam()).append("' must be not null"));
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNotNull().toThrow(anException);
     * </pre>
     * 
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<A, T> isNotNull(final CharSequence message, final Object... arguments) {
        return this.isNotNull(null, message, arguments);
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * Assertor.that(object).isNotNull().toThrow(anException);
     * </pre>
     * 
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return The operator
     */
    public Operator<A, T> isNotNull(final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> this.get() != null, null, message, arguments, locale);
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return the operator
     */
    public Operator<A, T> isEqual(final Object object) {
        return this.isEqual(object, this.msg(MSG.OBJECT.EQUALS, this.getParam(), this.getNextParam(1, object)));
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isEqual(final Object object, final CharSequence message, final Object... arguments) {
        return this.isEqual(object, null, message, arguments);
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isEqual(final Object object, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> this.isEqualInternal(object), null, message, arguments, locale, object);
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isNotEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final Object object) {
        return this.isNotEqual(object, this.msg(MSG.OBJECT.EQUALS + MSG.NOT, this.getParam(), this.getNextParam(1, object)));
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isNotEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final Object object, final CharSequence message, final Object... arguments) {
        return this.isNotEqual(object, null, message, arguments);
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * Assertor.that(object1).isNotEqual(object2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isNotEqual(final Object object, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(true, () -> !this.isEqualInternal(object), null, message, arguments, locale, object);
    }

    private boolean isEqualInternal(final Object object) {
        boolean result = false;
        if (this.get() == object) {
            result = true;
        } else if (this.get() != null && this.get().equals(object)) {
            result = true;
        } else if (ClassUtils.isAssignableFrom(CharSequence.class, this.get()) && ClassUtils.isAssignableFrom(CharSequence.class, object)
                && this.get().toString().equals(object.toString())) {
            result = true;
        }
        return result;
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assertor.that(object).is(Foo.class);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @return the operator
     * @see Class#isInstance
     */
    public Operator<A, T> isInstanceOf(final Class<?> type) {
        return this.isInstanceOf(type, this.msg(MSG.OBJECT.INSTANCE, this.getParam(), this.getNextParam(1, Constants.TYPE.CLASS)));
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assertor.that(object).is(Foo.class);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     * @see Class#isInstance
     */
    public Operator<A, T> isInstanceOf(final Class<?> type, final CharSequence message, final Object... arguments) {
        return this.isInstanceOf(type, null, message, arguments);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assertor.that(object).isInstanceOf(Foo.class, Locale.US, "Type doesn't match");
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     * @see Class#isInstance
     */
    public Operator<A, T> isInstanceOf(final Class<?> type, final Locale locale, final CharSequence message, final Object... arguments) {
        return this.combine(type != null && this.get() != null, () -> type.isInstance(this.get()),
                () -> this.msg(MSG.OBJECT.INSTANCE, true), message, arguments, locale, type);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @return the operator
     */
    public Operator<A, T> isAssignableFrom(final Class<?> type) {
        return this.isAssignableFrom(type, this.msg(MSG.OBJECT.ASSIGNABLE, this.getParam(), this.getNextParam(1, Constants.TYPE.UNKNOWN)));
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isAssignableFrom(final Class<?> type, final CharSequence message, final Object... arguments) {
        return this.isAssignableFrom(type, null, message, arguments);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> isAssignableFrom(final Class<?> type, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(this.get() != null && type != null, () -> type.isAssignableFrom(CastGenerics.getClass(this.get())),
                () -> this.msg(MSG.OBJECT.ASSIGNABLE, true), message, arguments, locale, type);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @return the operator
     */
    public Operator<A, T> matches(final Matcher<? super T> matcher) {
        final Description description = new StringDescription();
        description.appendText("Expected: ");
        description.appendDescriptionOf(matcher).appendText("\n     but: ");
        matcher.describeMismatch(this.get(), description);

        return this.matches(matcher, this.msg(MSG.OBJECT.MATCHES, this.getParam(), description.toString()));
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> matches(final Matcher<? super T> matcher, final CharSequence message, final Object... arguments) {
        return this.matches(matcher, null, message, arguments);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @return the operator
     */
    public Operator<A, T> matches(final Matcher<? super T> matcher, final Locale locale, final CharSequence message,
            final Object... arguments) {
        return this.combine(matcher != null, () -> matcher.matches(this.get()), () -> this.msg(MSG.OBJECT.MATCHES, true), message,
                arguments, locale);
    }

    /**
     * Validates the current object through a checker function (equivalent to
     * {@link java.util.function.Predicate}).
     * 
     * <pre>
     * Assertor.that(object).validates((Object obj) -&gt; {
     *     return obj != null;
     * }).toThrow();
     * 
     * Assertor.that("/var/log/dev.log").validates((path) -&gt; {
     *     return Paths.get(path).endsWith("dev.log");
     * }).isOK();
     * 
     * // Exceptions are catched, and returns a false result
     * Assertor.that("/var/log/dev.log").is((path) -&gt; {
     *     if (!new File(path).exists()) {
     *         throw new IOException();
     *     }
     *     return true;
     * }).isOK();
     * </pre>
     * 
     * @param predicate
     *            The predicate function
     * @param <E>
     *            The exception type
     * @return the operator
     */
    public <E extends Throwable> Operator<A, T> validates(final PredicateThrowable<T, E> predicate) {
        return this.validates(predicate, this.msg(MSG.OBJECT.VALIDATES, this.getParam()));
    }

    /**
     * Validates the current object through a checker function (equivalent to
     * {@link java.util.function.Predicate}).
     * 
     * <pre>
     * Assertor.that(object).validates((Object obj) -&gt; {
     *     return obj != null;
     * }, "Object is null!!!").toThrow();
     * 
     * Assertor.that("/var/log/dev.log").validates((path) -&gt; {
     *     return Paths.get(path).endsWith("dev.log");
     * }, "Path is invalid").isOK();
     * 
     * // Exceptions are catched, and returns a false result
     * Assertor.that("/var/log/dev.log").validates((path) -&gt; {
     *     if (!new File(path).exists()) {
     *         throw new IOException();
     *     }
     *     return true;
     * }, "Path '%1$s*' provided by '%s' is invalid", user).isOK();
     * </pre>
     * 
     * @param predicate
     *            The predicate function
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @param <E>
     *            The exception type
     * @return the operator
     */
    public <E extends Throwable> Operator<A, T> validates(final PredicateThrowable<T, E> predicate, final CharSequence message,
            final Object... arguments) {
        return this.validates(predicate, null, message, arguments);
    }

    /**
     * Validates the current object through a checker function (equivalent to
     * {@link java.util.function.Predicate}).
     * 
     * <pre>
     * Assertor.that(object).validates((Object obj) -&gt; {
     *     return obj != null;
     * }, Locale.US, "Object is null!!!").toThrow();
     * 
     * Assertor.that("/var/log/dev.log").validates((path) -&gt; {
     *     return Paths.get(path).endsWith("dev.log");
     * }, Locale.US, "Path is invalid").isOK();
     * 
     * // Exceptions are catched, and returns a false result
     * Assertor.that("/var/log/dev.log").validates((path) -&gt; {
     *     if (!new File(path).exists()) {
     *         throw new IOException();
     *     }
     *     return true;
     * }, Locale.US, "Path '%1$s*' provided by '%s' is invalid", user).isOK();
     * 
     * // If we replace this last line by:
     * }, Locale.US, "Path '%1$s*' provided by '%s' is invalid ('%.2f'ms"), "John", 10.26589f).getErrors();
     * // result =&gt; "Path '/var/log/dev.log' provided by 'John' is invalid ('10.27'ms)
     * 
     * // With France locale (in decimal number the dot is replaced by a comma)
     * }, Locale.FRANCE, "Path '%1$s*' provided by '%s' is invalid ('%.2f'ms)", "John", 10.26589f).getErrors();
     * // result =&gt; "Path '/var/log/dev.log' provided by 'John' is invalid ('10,27'ms)
     * 
     * </pre>
     * 
     * @param predicate
     *            The predicate function
     * @param locale
     *            The locale of the message (only applied for this message,
     *            otherwise use {@link Assertor#setLocale})
     * @param message
     *            The message on mismatch
     * @param arguments
     *            The arguments of the message, use {@link String#format}
     * @param <E>
     *            The exception type
     * @return the operator
     */
    public <E extends Throwable> Operator<A, T> validates(final PredicateThrowable<T, E> predicate, final Locale locale,
            final CharSequence message, final Object... arguments) {
        try {
            return this.combine(predicate != null, () -> predicate.testThrows(this.get()), () -> this.msg(MSG.OBJECT.VALIDATES, true),
                    message, arguments, locale);
        } catch (Throwable e) {
            return this.combine(true, () -> false, null, message, arguments, locale);
        }
    }
}