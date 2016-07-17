/*-
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

import java.util.Calendar;
import java.util.Date;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.commons.lang3.ArrayUtils;

import fr.landel.utils.commons.NumberUtils;
import fr.landel.utils.commons.StringUtils;

/**
 * Assertor operator. Manage the combining and the result as {@code String} or
 * as {@code Throwable}.
 *
 * @since 1 juil. 2016
 * @author Gilles
 *
 * @param <A>
 *            The assertor type
 * @param <T>
 *            The type of checked object
 */
public class Operator<A extends AssertObject<A, T>, T> {

    private static final Pattern PATTERN_PARAMETERS = Pattern.compile("(%(\\d+\\$)?p)");

    private final A assertor;

    /**
     * Constructor
     *
     * @param assertor
     *            The linked assertor
     */
    public Operator(final A assertor) {
        this.assertor = assertor;
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A and() {
        this.assertor.setCondition(Assertor.AND);

        return this.assertor;
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> and(final N object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> and(final Map<K, V> object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> and(final I object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> and(final Z[] object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> and(final Z object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate and(final Date object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar and(final Calendar object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean and(final Boolean object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> and(final Class<Z> object) {
        return this.condition(Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'and' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X and(final Z object) {
        return this.condition((X) Assertor.that(object), Assertor.AND);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A or() {
        this.assertor.setCondition(Assertor.OR);

        return this.assertor;
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> or(final N object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> or(final Map<K, V> object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> or(final I object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> or(final Z[] object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> or(final Z object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate or(final Date object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar or(final Calendar object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean or(final Boolean object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> or(final Class<Z> object) {
        return this.condition(Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'or' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X or(final Z object) {
        return this.condition((X) Assertor.that(object), Assertor.OR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one.
     * 
     * @return the linked assertor
     */
    public A xor() {
        this.assertor.setCondition(Assertor.XOR);

        return this.assertor;
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <N>
     *            The number to check
     * @return the new assertor
     */
    public <N extends Number & Comparable<N>> AssertNumber<N> xor(final N object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <K>
     *            The type of key to check
     * @param <V>
     *            The type of value to check
     * @return the new assertor
     */
    public <K, V> AssertMap<K, V> xor(final Map<K, V> object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <I>
     *            The type of the iterable to check
     * @param <Z>
     *            The generic type of the iterable to check
     * @return the new assertor
     */
    public <I extends Iterable<Z>, Z> AssertIterable<I, Z> xor(final I object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertArray<Z> xor(final Z[] object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z extends CharSequence> AssertCharSequence<Z> xor(final Z object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertDate xor(final Date object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertCalendar xor(final Calendar object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @return the new assertor
     */
    public AssertBoolean xor(final Boolean object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    public <Z> AssertClass<Z> xor(final Class<Z> object) {
        return this.condition(Assertor.that(object), Assertor.XOR);
    }

    /**
     * Appends the 'xor' operator between the previous condition and the next
     * one. Creates a new assertor for the new object to check.
     * 
     * @param object
     *            The object to check
     * @param <X>
     *            The assertor object
     * @param <Z>
     *            The type to check
     * @return the new assertor
     */
    @SuppressWarnings("unchecked")
    public <X extends AssertObject<X, Z>, Z> X xor(final Z object) {
        return this.condition((X) Assertor.that(object), Assertor.XOR);
    }

    @SuppressWarnings("unchecked")
    private <X extends AssertObject<X, Z>, Z> X condition(final X assertor, final int previousCondition) {
        assertor.setCondition(previousCondition);
        assertor.combine((X) this.assertor);

        return assertor;
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch
     * 
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow() {
        this.toThrow(this.assertor.getMessage().append("."), new Object[] {});
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message.
     * 
     * @param message
     *            The message to throw on mismatch
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final String message) {
        this.toThrow(message, new Object[] {});
    }

    /**
     * Throws an {@code IllegalArgumentException} on assertions mismatch. Set
     * the exception with the specified message.
     * 
     * @param message
     *            The message to throw on mismatch
     * @param arguments
     *            The message arguments
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public void toThrow(final CharSequence message, final Object... arguments) {
        boolean valid = this.assertor.isValid();
        Object[] parameters = ArrayUtils.clone(this.assertor.getParameters());
        this.assertor.clear();
        if (!valid) {
            Operator.manageExceptions(Assertor.getAssertionPrefix(), null, message, parameters, arguments);
        }
    }

    /**
     * Throws the specified exception on assertions mismatch. The internal
     * exception is appended to the specified one as suppressed.
     * 
     * @param exception
     *            The exception to throw on mismatch
     * @param <E>
     *            The exception type
     * @throws E
     *             The thrown exception
     * @throws IllegalArgumentException
     *             On assertions mismatch
     */
    public <E extends Throwable> void toThrow(final E exception) throws E {
        boolean valid = this.assertor.isValid();
        this.assertor.clear();
        if (!valid) {
            Operator.manageExceptions(Assertor.getAssertionPrefix(), exception, null, null, null);
        }
    }

    /**
     * @return The result of the assertion combining
     */
    public boolean getResult() {
        boolean valid = this.assertor.isValid();
        this.assertor.clear();
        return valid;
    }

    /**
     * Manages exceptions (between raised a specific exception or the standard
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
     * Gets the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %p or %1$p
     * 
     * <pre>
     * Operator.getMessage(10, 20, &quot;The number '%p' is not greater than number '%p'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * Operator.getMessage(10, 20, &quot;'%2$p' &gt; '%1$p'&quot;);
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
                msg = String.format(Assertor.getLocale(), msg, arguments);
            }
        } else {
            msg = defaultString.toString();
        }
        return new StringBuilder().append(Assertor.getAssertionPrefix()).append(msg).toString();
    }
}