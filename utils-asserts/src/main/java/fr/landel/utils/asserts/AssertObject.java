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
package fr.landel.utils.asserts;

import java.util.regex.Pattern;

import org.hamcrest.Description;
import org.hamcrest.Matcher;
import org.hamcrest.StringDescription;

import fr.landel.utils.commons.NumberUtils;
import fr.landel.utils.commons.StringUtils;

/**
 * Assertion utility class that assists in validating arguments for objects.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <T>
 *            the class type
 */
public class AssertObject<T extends AssertObject<T, O>, O> {

    private static final Pattern PATTERN_PARAMETERS = Pattern.compile("(%(\\d+\\$)?p)");

    private O object;

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     */
    protected AssertObject(final O object) {
        this.object = object;
    }

    /**
     * @return this
     */
    @SuppressWarnings("unchecked")
    protected T getThis() {
        return (T) this;
    }

    /**
     * @return the object to check
     */
    protected O get() {
        return this.object;
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNull();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public T isNull() {
        return this.isNull((CharSequence) null);
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNull(&quot;The value must be null&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public T isNull(final CharSequence message, final Object... arguments) {
        isNull(this.object, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNull(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            the type of exception
     * @throws E
     *             if the object is not {@code null}
     */
    public <E extends Throwable> T isNull(final E exception) throws E {
        isNull(this.object, exception, null);

        return this.getThis();
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <E>
     *            the type of exception
     * @throws E
     *             if the object is not {@code null}
     */
    protected static <E extends Throwable> void isNull(final Object object, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (object != null) {
            manageExceptions("the object argument must be null", exception, message, new Object[] {object}, arguments);
        }
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNotNull();
     * </pre>
     * 
     * @return this
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public T isNotNull() {
        return this.isNotNull((CharSequence) null);
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNotNull(&quot;The object must not be null&quot;);
     * </pre>
     * 
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public T isNotNull(final CharSequence message, final Object... arguments) {
        isNotNull(this.object, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * <pre>
     * AssertUtils.check(object).isNotNull(exceptionToThrowOnError);
     * </pre>
     * 
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is {@code null}
     */
    public <E extends Throwable> T isNotNull(final E exception) throws E {
        isNotNull(this.object, exception, null);

        return this.getThis();
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is {@code null}
     */
    protected static <E extends Throwable> void isNotNull(final Object object, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (object == null) {
            manageExceptions("this argument is required; it must not be null", exception, message, new Object[] {object}, arguments);
        }
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * AssertUtils.check(object).isNotEqual(foo1, foo2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return this
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public T isNotEqual(final Object object) {
        return this.isNotEqual(object, (CharSequence) null);
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * AssertUtils.check(object).isNotEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public T isNotEqual(final Object object, final CharSequence message, final Object... arguments) {
        isNotEqual(this.object, object, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that the first object is not equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * AssertUtils.check(object).isNotEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if both are {@code null} or if objects are not equal. The
     *             standard exception is appended as suppressed.
     */
    public <E extends Throwable> T isNotEqual(final Object object, final E exception) throws E {
        isNotEqual(this.object, object, exception, null);

        return this.getThis();
    }

    protected static <E extends Throwable> void isNotEqual(final Object obj1, final Object obj2, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        if (obj1 == null && obj2 == null) {
            manageExceptions("Both objects are null.", exception, message, new Object[] {obj1, obj2}, arguments);
        } else if (obj1 != null && obj2 != null) {
            if (obj1.equals(obj2)) {
                manageExceptions("Object1 is equal to Object2.", exception, message, new Object[] {obj1, obj2}, arguments);
            } else if (CharSequence.class.isAssignableFrom(obj1.getClass()) && CharSequence.class.isAssignableFrom(obj2.getClass())
                    && obj1.toString().equals(obj2.toString())) {
                manageExceptions("CharSequence1 is equal to CharSequence2.", exception, message, new Object[] {obj1, obj2}, arguments);
            }
        }
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * AssertUtils.check(object).isEqual(foo1, foo2);
     * </pre>
     * 
     * @param object
     *            the second object
     * @return this
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and if objects are not
     *             equal.
     */
    public T isEqual(final Object object) {
        return this.isEqual(object, (CharSequence) null);
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * 
     * <pre>
     * AssertUtils.check(object).isEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and not the other one or
     *             are not equal.
     */
    public T isEqual(final Object object, final CharSequence message, final Object... arguments) {
        isEqual(this.object, object, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that the first object is equal to the second one. (supports
     * {@code CharSequence} implementation like StringBuilder)
     * 
     * <pre>
     * AssertUtils.check(object).isEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one object is {@code null} and if objects are not
     *             equal. The standard exception is appended as suppressed.
     */
    public <E extends Throwable> T isEqual(final Object object, final E exception) throws E {
        isEqual(this.object, object, exception, null);

        return this.getThis();
    }

    protected static <E extends Throwable> void isEqual(final Object obj1, final Object obj2, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            if (!CharSequence.class.isAssignableFrom(obj1.getClass()) || !CharSequence.class.isAssignableFrom(obj1.getClass())
                    || !obj1.toString().equals(obj2.toString())) {
                manageExceptions("Object1 is not equal to Object2.", exception, message, new Object[] {obj1, obj2}, arguments);
            }
        } else if (obj1 == null && obj2 != null) {
            manageExceptions("Object1 is null but not Object2.", exception, message, new Object[] {obj1, obj2}, arguments);
        } else if (obj1 != null && obj2 == null) {
            manageExceptions("Object2 is null but not Object1.", exception, message, new Object[] {obj1, obj2}, arguments);
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * AssertUtils.check(object).instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param clazz
     *            the required class
     * @return this
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public T isInstanceOf(final Class<?> clazz) {
        return this.isInstanceOf(clazz, (CharSequence) null);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * AssertUtils.check(object).instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public T isInstanceOf(final Class<?> type, final CharSequence message, final Object... arguments) {
        isInstanceOf(this.object, type, null, message, arguments);

        return this.getThis();
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * AssertUtils.check(object).instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     * @see Class#isInstance
     */
    public <E extends Throwable> T isInstanceOf(final Class<?> type, final E exception) throws E {
        isInstanceOf(this.object, type, exception, null);

        return this.getThis();
    }

    protected static <E extends Throwable> void isInstanceOf(final Object obj, final Class<?> type, final E exception,
            final CharSequence message, final Object... arguments) throws E {
        isNotNull(type, null, "Type to check against must not be null", exception);
        if (!type.isInstance(obj)) {

            final String clazzName = getClassName(obj);

            manageExceptions(
                    new StringBuilder().append("Object of class [").append(clazzName).append("] must be an instance of ").append(type),
                    exception, message, new Object[] {type, obj}, arguments);
        }
    }

    /**
     * Get the class name
     * 
     * @param obj
     *            the object
     * @return the class name or the string "null"
     */
    private static String getClassName(final Object obj) {
        final String clazzName;
        if (obj != null) {
            clazzName = obj.getClass().getName();
        } else {
            clazzName = "null";
        }
        return clazzName;
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param type
     *            the type to check
     * @return this
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public T isAssignable(final Class<?> type) {
        return this.isAssignable(type, (CharSequence) null);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format) message looks
     * @return this OK when appended to it.
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public T isAssignable(final Class<?> type, final CharSequence message, final Object... arguments) {
        this.isNotNull();

        AssertClass.isAssignable(type, this.object.getClass(), null, message, arguments);

        return this.getThis();
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
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the classes are not assignable. The standard exception is
     *             appended as suppressed.
     */
    public <E extends Throwable> T isAssignable(final Class<?> type, final E exception) throws E {
        this.isNotNull();

        AssertClass.isAssignable(type, this.object.getClass(), exception, null);

        return this.getThis();
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @return this
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public T that(final Matcher<? super O> matcher) {
        return this.that(matcher, (String) null);
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @param message
     *            a message, if {@code null} use the default assertion message
     *            (%p or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @return this
     * @throws IllegalArgumentException
     *             if matcher doesn't match
     */
    public T that(final Matcher<? super O> matcher, final String message, final Object... arguments) {
        that(this.object, matcher, null, message, arguments);

        return this.getThis();
    }

    /**
     * The equivalent of JUnit/hamcrest assertThat
     * 
     * @param matcher
     *            an hamcrest matcher
     * @param exception
     *            the exception to throw on error
     * @return this
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     */
    public <E extends Throwable> T that(final Matcher<? super O> matcher, final E exception) throws E {
        that(this.object, matcher, exception, null);

        return this.getThis();
    }

    protected static <O, E extends Throwable> void that(final O actual, final Matcher<? super O> matcher, final E exception,
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
     * Manage exceptions (between raised a specific exception or the standard
     * IllegalArgumentException)
     * 
     * @param defaultString
     *            The default message
     * @param exception
     *            The specific exception
     * @param message
     *            The message for the IllegalArgumentException
     * @param parameters
     *            The method parameters
     * @param arguments
     *            The arguments for the IllegalArgumentException
     * @param <E>
     *            The specific exception type
     * @throws E
     *             If exception parameter is set
     */
    protected static <E extends Throwable> void manageExceptions(final CharSequence defaultString, final E exception,
            final CharSequence message, final Object[] parameters, final Object[] arguments) throws E {
        if (exception != null) {
            exception.addSuppressed(new IllegalArgumentException(defaultString.toString()));
            throw exception;
        } else {
            throw new IllegalArgumentException(getMessage(defaultString, message, parameters, arguments));
        }
    }

    /**
     * Get the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %p or %1$p
     * 
     * <pre>
     * AssertUtils.getMessage(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * AssertUtils.getMessage(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
     * // Exception: "'20' &gt; '10'"
     * </pre>
     * 
     * @param defaultString
     *            The default message provided by each method
     * @param message
     *            The user message
     * @param parameters
     *            The method parameters
     * @param arguments
     *            The user arguments
     * @return The message formatted
     */
    protected static String getMessage(final CharSequence defaultString, final CharSequence message, final Object[] parameters,
            final Object[] arguments) {
        String msg;
        String group;
        String replacement = null;
        int number;

        if (StringUtils.isNotEmpty(message)) {
            msg = message.toString();
            if (parameters != null && parameters.length > 0) {
                java.util.regex.Matcher matcher;
                int count = 0;
                while ((matcher = PATTERN_PARAMETERS.matcher(msg)).find()) {
                    group = matcher.group(0);

                    if (group.indexOf('$') > -1) {
                        number = NumberUtils.parseInt(StringUtils.remove(StringUtils.remove(group, '%'), "$p"), 0);
                        if (number > 0 && number <= parameters.length) {
                            replacement = String.valueOf(parameters[number - 1]);
                        } else {
                            replacement = "";
                        }
                    } else if (count < parameters.length) {
                        replacement = String.valueOf(parameters[count]);
                        count++;
                    } else {
                        replacement = "";
                    }
                    msg = StringUtils.replace(msg, replacement, matcher.start(), matcher.end());
                }
            }
            if (arguments != null && arguments.length > 0) {
                msg = String.format(AssertUtils.getLocale(), msg, arguments);
            }
        } else {
            msg = defaultString.toString();
        }
        return new StringBuilder().append(AssertUtils.getAssertionPrefix()).append(msg).toString();
    }
}