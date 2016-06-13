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

/**
 * Assertion utility class that assists in validating arguments for objects.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <T>
 *            the class type
 */
public class AssertMultipleObject<T extends AssertMultipleObject<T, O>, O> {

    private O[] objects;

    /**
     * 
     * Constructor
     *
     * @param objects
     *            The object to check
     */
    protected AssertMultipleObject(O[] objects) {
        this.objects = objects;
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
    protected O[] get() {
        return this.objects;
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
    public T areNull() {
        return this.areNull((CharSequence) null);
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
    public T areNull(final CharSequence message, final Object... arguments) {
        areNull(this.objects, null, message, arguments);

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
     * @param <E>
     *            the type of exception
     * @return this
     * @throws E
     *             if the object is not {@code null}
     */
    public <E extends Throwable> T areNull(final E exception) throws E {
        areNull(this.objects, exception, null);

        return this.getThis();
    }

    /**
     * Assert that an object is {@code null}.
     * 
     * @param objects
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
    protected static <E extends Throwable> void areNull(final Object[] objects, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (objects != null) {
            boolean found = false;
            for (Object obj : objects) {
                if (obj != null) {
                    found = true;
                    break;
                }
            }
            if (found) {
                AssertObject.manageExceptions("these arguments must be null", exception, message, objects, arguments);
            }
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
    public T areNotNull() {
        return this.areNotNull((CharSequence) null);
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
    public T areNotNull(final CharSequence message, final Object... arguments) {
        areNotNull(this.objects, null, message, arguments);

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
    public <E extends Throwable> T areNotNull(final E exception) throws E {
        areNotNull(this.objects, exception, null);

        return this.getThis();
    }

    /**
     * Assert that an object is not {@code null}.
     * 
     * @param objects
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
    protected static <E extends Throwable> void areNotNull(final Object[] objects, final E exception, final CharSequence message,
            final Object... arguments) throws E {
        if (objects == null) {
            AssertObject.manageExceptions("these arguments are required; they must not be null", exception, message, new Object[] {objects},
                    arguments);
        } else {
            boolean found = false;
            for (Object obj : objects) {
                if (obj == null) {
                    found = true;
                    break;
                }
            }
            if (found) {
                AssertObject.manageExceptions("these arguments are required; they must not be null", exception, message, objects,
                        arguments);
            }
        }
    }
}