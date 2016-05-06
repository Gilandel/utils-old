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
package org.gl.utils.commons;

import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Abstract part see Assert class.
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
public abstract class AbstractAssert {

    private static final String ASSERTION_FAILED = "[Assertion failed]";
    private static final Pattern PATTERN_PARAMETERS = Pattern.compile("(%(\\d+\\$)?p)");

    private static Locale locale = Locale.US;

    /**
     * @return the locale
     */
    public static final Locale getLocale() {
        return AbstractAssert.locale;
    }

    /**
     * @param locale
     *            the locale to set
     */
    public static final void setLocale(final Locale locale) {
        AbstractAssert.locale = locale;
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * Assert.isFalse(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression) {
        isFalse(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * Assert.isFalse(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isFalse(final boolean expression, final String message, final Object... arguments) {
        if (expression) {
            throw new IllegalArgumentException(getMessage("this expression must be false", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code true}.
     * 
     * <pre>
     * Assert.isFalse(i &gt; 0, exceptionToThrowOnError);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public static <E extends Throwable> void isFalse(final boolean expression, final E exception) throws E {
        if (expression) {
            exception.addSuppressed(new IllegalArgumentException("this expression must be false"));
            throw exception;
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression) {
        isTrue(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0, &quot;The value must be greater than zero&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if expression is {@code false}
     */
    public static void isTrue(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalArgumentException(getMessage("this expression must be true", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalArgumentException} if
     * the test result is {@code false}.
     * 
     * <pre>
     * Assert.isTrue(i &gt; 0, exceptionToThrowOnError);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}
     */
    public static <E extends Throwable> void isTrue(final boolean expression, final E exception) throws E {
        if (!expression) {
            exception.addSuppressed(new IllegalArgumentException("this expression must be true"));
            throw exception;
        }
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object) {
        isNull(object, (String) null);
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value, &quot;The value must be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not {@code null}
     */
    public static void isNull(final Object object, final String message, final Object... arguments) {
        if (object != null) {
            throw new IllegalArgumentException(getMessage("the object argument must be null", message, new Object[] {object}, arguments));
        }
    }

    /**
     * Assert that an object is {@code null} .
     * 
     * <pre>
     * Assert.isNull(value, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is not {@code null}
     */
    public static <E extends Throwable> void isNull(final Object object, final E exception) throws E {
        if (object != null) {
            exception.addSuppressed(new IllegalArgumentException("the object argument must be null"));
            throw exception;
        }
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(object);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object) {
        isNotNull(object, (String) null);
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(clazz, &quot;The class must not be null&quot;);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     */
    public static void isNotNull(final Object object, final String message, final Object... arguments) {
        if (object == null) {
            throw new IllegalArgumentException(
                    getMessage("this argument is required; it must not be null", message, new Object[] {object}, arguments));
        }
    }

    /**
     * Assert that an object is not {@code null} .
     * 
     * <pre>
     * Assert.isNotNull(clazz, exceptionToThrowOnError);
     * </pre>
     * 
     * @param object
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the object is {@code null}
     */
    public static <E extends Throwable> void isNotNull(final Object object, final E exception) throws E {
        if (object == null) {
            exception.addSuppressed(new IllegalArgumentException("this argument is required; it must not be null"));
            throw exception;
        }
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
     * Assert.isNotEqual(foo1, foo2);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public static void isNotEqual(final Object obj1, final Object obj2) {
        AbstractAssert.isNotEqual(obj1, obj2, (String) null);
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
     * Assert.isNotEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if both objects are {@code null} or are equal.
     */
    public static void isNotEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 == null && obj2 == null) {
            throw new IllegalArgumentException(getMessage("Both objects are null.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 != null && obj2 != null && obj1.equals(obj2)) {
            throw new IllegalArgumentException(getMessage("Object1 is equal to Object2.", message, new Object[] {obj1, obj2}, arguments));
        }
    }

    /**
     * Assert that the first object is not equal to the second one.
     * 
     * <pre>
     * Assert.isNotEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if both are {@code null} or if objects are not equal. The
     *             standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isNotEqual(final Object obj1, final Object obj2, final E exception) throws E {
        if (obj1 == null && obj2 == null) {
            exception.addSuppressed(new IllegalArgumentException("Both objects are null."));
            throw exception;
        } else if (obj1 != null && obj2 != null && obj1.equals(obj2)) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is equal to Object2."));
            throw exception;
        }
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and if objects are not
     *             equal.
     */
    public static void isEqual(final Object obj1, final Object obj2) {
        AbstractAssert.isEqual(obj1, obj2, (String) null);
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2, message);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if only one object is {@code null} and not the other one or
     *             are not equal.
     */
    public static void isEqual(final Object obj1, final Object obj2, final String message, final Object... arguments) {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            throw new IllegalArgumentException(
                    getMessage("Object1 is not equal to Object2.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 == null && obj2 != null) {
            throw new IllegalArgumentException(
                    getMessage("Object1 is null but not Object2.", message, new Object[] {obj1, obj2}, arguments));
        } else if (obj1 != null && obj2 == null) {
            throw new IllegalArgumentException(
                    getMessage("Object2 is null but not Object1.", message, new Object[] {obj1, obj2}, arguments));
        }
    }

    /**
     * Assert that the first object is equal to the second one.
     * 
     * <pre>
     * Assert.isEqual(foo1, foo2, exceptionToThrowOnError);
     * </pre>
     * 
     * @param obj1
     *            the first object
     * @param obj2
     *            the second object
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if at least one object is {@code null} and if objects are not
     *             equal. The standard exception is appended as suppressed.
     */
    public static <E extends Throwable> void isEqual(final Object obj1, final Object obj2, final E exception) throws E {
        if (obj1 != null && obj2 != null && !obj1.equals(obj2)) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is not equal to Object2."));
            throw exception;
        } else if (obj1 == null && obj2 != null) {
            exception.addSuppressed(new IllegalArgumentException("Object1 is null but not Object2."));
            throw exception;
        } else if (obj1 != null && obj2 == null) {
            exception.addSuppressed(new IllegalArgumentException("Object2 is null but not Object1."));
            throw exception;
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param clazz
     *            the required class
     * @param obj
     *            the object to check
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public static void isInstanceOf(final Class<?> clazz, final Object obj) {
        isInstanceOf(clazz, obj, (String) null);
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param obj
     *            the object to check
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalArgumentException
     *             if the object is not an instance of clazz
     * @see Class#isInstance
     */
    public static void isInstanceOf(final Class<?> type, final Object obj, final String message, final Object... arguments) {
        isNotNull(type, "Type to check against must not be null");
        if (!type.isInstance(obj)) {

            final String clazzName = getClassName(obj);

            throw new IllegalArgumentException(getMessage("Object of class [" + clazzName + "] must be an instance of " + type, message,
                    new Object[] {type, obj}, arguments));
        }
    }

    /**
     * Assert that the provided object is an instance of the provided class.
     * 
     * <pre>
     * Assert.instanceOf(Foo.class, foo);
     * </pre>
     * 
     * @param type
     *            the type to check against
     * @param obj
     *            the object to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if condition doesn't match.
     * @see Class#isInstance
     */
    public static <E extends Throwable> void isInstanceOf(final Class<?> type, final Object obj, final E exception) throws E {
        isNotNull(type, "Type to check against must not be null", exception);
        if (!type.isInstance(obj)) {

            final String clazzName = getClassName(obj);

            exception.addSuppressed(new IllegalArgumentException("Object of class [" + clazzName + "] must be an instance of " + type));
            throw exception;
        }
    }

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
     * @param superType
     *            the super type to check
     * @param subType
     *            the sub type to check
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public static void isAssignable(final Class<?> superType, final Class<?> subType) {
        isAssignable(superType, subType, (String) null);
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass);
     * </pre>
     * 
     * @param superType
     *            the super type to check against
     * @param subType
     *            the sub type to check
     * @param message
     *            the exception message, use the default assertion if null (%p
     *            or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format) message looks
     *            OK when appended to it.
     * @throws IllegalArgumentException
     *             if the classes are not assignable
     */
    public static void isAssignable(final Class<?> superType, final Class<?> subType, final String message, final Object... arguments) {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            throw new IllegalArgumentException(
                    getMessage(subType + " is not assignable to " + superType, message, new Object[] {superType, subType}, arguments));
        }
    }

    /**
     * Assert that {@code superType.isAssignableFrom(subType)} is {@code true}.
     * 
     * <pre>
     * Assert.isAssignable(Number.class, myClass), exceptionToThrowOnError);
     * </pre>
     * 
     * @param superType
     *            the super type to check against
     * @param subType
     *            the sub type to check
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if the classes are not assignable. The standard exception is
     *             appended as suppressed.
     */
    public static <E extends Throwable> void isAssignable(final Class<?> superType, final Class<?> subType, final E exception) throws E {
        isNotNull(superType, "Type to check against must not be null");
        if (subType == null || !superType.isAssignableFrom(subType)) {
            exception.addSuppressed(new IllegalArgumentException(subType + " is not assignable to " + superType));
            throw exception;
        }
    }

    /**
     * Assert a boolean expression, throwing {@link IllegalStateException} if
     * the test result is {@code false}.
     * <p>
     * Call {@link #isTrue(boolean)} if you wish to throw
     * {@link IllegalArgumentException} on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @throws IllegalStateException
     *             if the supplied expression is {@code false}
     */
    public static void state(final boolean expression) {
        state(expression, (String) null);
    }

    /**
     * Assert a boolean expression, throwing {@code IllegalStateException} if
     * the test result is {@code false}. Call isTrue if you wish to throw
     * IllegalArgumentException on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null, &quot;The id property must not already be initialized&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param message
     *            the exception message to use if the assertion fails (%p or
     *            %1$p can be used to display parameter value, see explanation
     *            in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             if expression is {@code false}
     */
    public static void state(final boolean expression, final String message, final Object... arguments) {
        if (!expression) {
            throw new IllegalStateException(getMessage("this state invariant must be true", message, new Object[] {expression}, arguments));
        }
    }

    /**
     * Assert a boolean expression, appending {@code IllegalStateException} if
     * the test result is {@code false}. Call isTrue if you wish to throw
     * IllegalArgumentException on an assertion failure.
     * 
     * <pre>
     * Assert.state(id == null, &quot;The id property must not already be initialized&quot;);
     * </pre>
     * 
     * @param expression
     *            a boolean expression
     * @param exception
     *            the exception to throw on error
     * @param <E>
     *            The type of exception
     * @throws E
     *             if expression is {@code false}. The standard exception is
     *             appended as suppressed.
     */
    public static <E extends Throwable> void state(final boolean expression, final E exception) throws E {
        if (!expression) {
            exception.addSuppressed(new IllegalStateException("this state invariant must be true"));
            throw exception;
        }
    }

    /**
     * Fail, throwing {@link IllegalStateException}.
     * 
     * <pre>
     * Assert.fail(&quot;Error&quot;, new Exception());
     * </pre>
     * 
     * @param message
     *            a message, if {@code null} use the default assertion message
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             in all cases
     */
    public static void fail(final String message, final Object... arguments) {
        fail(null, message, arguments);
    }

    /**
     * Fail, throwing {@link IllegalStateException}.
     * 
     * <pre>
     * Assert.fail(&quot;Error&quot;, new Exception());
     * </pre>
     * 
     * 
     * @param throwable
     *            the cause exception
     * @param message
     *            a message, if {@code null} use the default assertion message
     *            (%p or %1$p can be used to display parameter value, see
     *            explanation in the class description)
     * @param arguments
     *            the message arguments (use with String.format)
     * @throws IllegalStateException
     *             in all cases
     */
    public static void fail(final Throwable throwable, final String message, final Object... arguments) {
        if (throwable != null) {
            throw new IllegalArgumentException(getMessage("", message, new Object[] {throwable}, arguments), throwable);
        } else {
            throw new IllegalArgumentException(getMessage("", message, new Object[] {throwable}, arguments));
        }
    }

    protected static String getMessage(final String defaultString, final String message, final Object[] parameters,
            final Object[] arguments) {
        String msg;
        String group;
        String replacement = null;
        int number;

        if (StringUtils.isNotEmpty(message)) {
            msg = message;
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
                    if (replacement != null) {
                        msg = StringUtils.replace(msg, replacement, matcher.start(), matcher.end());
                        replacement = null;
                    }
                }
            }
            if (arguments != null && arguments.length > 0) {
                msg = String.format(AbstractAssert.locale, msg, arguments);
            }
        } else {
            msg = defaultString;
        }
        return ASSERTION_FAILED + " " + msg;
    }
}