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

import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.Map;
import java.util.function.Supplier;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.commons.CastGenerics;
import fr.landel.utils.commons.EnumChar;
import fr.landel.utils.commons.NumberUtils;
import fr.landel.utils.commons.function.BooleanSupplierThrowable;

/**
 * Abstract class with core features for validating assertion.
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 * @param <A>
 *            the assert class type
 * @param <T>
 *            The object type
 */
public abstract class AbstractAssertObject<A extends AbstractAssertObject<A, T>, T> extends Constants {

    private final T object;
    private final int type;
    private final Class<T> clazz;
    private final Operator<A, T> operator;
    private int paramIndex;
    private StringBuilder param;
    private Object[] parameters;
    private int condition;
    private boolean valid = true;
    private boolean prerequisitesError;
    private StringBuilder message;

    private AbstractAssertObject<?, ?> previousAssertor;
    private Boolean previousValid;
    private boolean previousPrerequisitesError;
    private int previousCondition = AND;
    private StringBuilder previousMessage;

    private boolean not;
    private boolean notPersonalized;

    /**
     * 
     * Constructor
     *
     * @param object
     *            The object to check
     * @param type
     *            The object type
     */
    @SuppressWarnings("unchecked")
    protected AbstractAssertObject(final T object, final int type) {
        this.object = object;
        this.clazz = CastGenerics.getClass(object);
        this.operator = new Operator<>((A) this);
        this.parameters = new Object[] {object};
        this.paramIndex = 1;
        this.type = type;
        this.param = getParam(this.paramIndex, type);
        this.message = new StringBuilder();
    }

    /**
     * Clear the assertor and its antecedents
     */
    protected void clear() {
        this.parameters = new Object[] {this.object};
        this.paramIndex = 1;
        this.param = getParam(this.paramIndex, this.type);
        this.message = new StringBuilder();
        this.valid = true;
        this.condition = AND;
        this.not = false;
        this.notPersonalized = false;
        this.prerequisitesError = false;

        if (this.previousAssertor != null) {
            this.previousAssertor.clear();
            this.previousAssertor = null;
            this.previousValid = null;
            this.previousPrerequisitesError = false;
            this.previousCondition = AND;
            this.previousMessage = null;
        }
    }

    /**
     * @return the parameter string (example for the first one: %1$s*)
     */
    protected StringBuilder getParam() {
        return this.param;
    }

    /**
     * @return The previous condition value
     */
    protected int getCondition() {
        return this.condition;
    }

    /**
     * @param condition
     *            The previous condition value
     */
    protected void setCondition(final int condition) {
        this.condition = condition;
    }

    /**
     * @return if the prerequisites are in error
     */
    protected boolean isPrerequisitesError() {
        return this.prerequisitesError;

    }

    /**
     * Only call this at the end of each assertor condition
     * 
     * @return if the condition is valid
     */
    protected boolean isValid() {
        if (!this.previousPrerequisitesError && !this.prerequisitesError) {
            if (this.previousValid != null) {
                return check(this.previousValid, this.previousCondition, this.valid);
            } else {
                return this.valid;
            }
        }
        return false;
    }

    /**
     * @return the operator
     */
    protected Operator<A, T> getOperator() {
        return this.operator;
    }

    /**
     * @return the object to check
     */
    protected T get() {
        return this.object;
    }

    /**
     * @return this
     */
    @SuppressWarnings("unchecked")
    protected A getThis() {
        return (A) this;
    }

    /**
     * @return the class object to check
     */
    protected Class<T> getObjectClass() {
        return this.clazz;
    }

    /**
     * Get the message and define that the current condition uses a personalized
     * message, not the default one
     * 
     * @param key
     *            The message key
     * @param arguments
     *            The arguments to replace in message
     * @return The loaded property
     */
    protected CharSequence msg(final CharSequence key, final CharSequence... arguments) {
        return this.msg(key, false, arguments);
    }

    /**
     * Get the message and define that the current condition uses a personalized
     * message, not the default one
     * 
     * @param key
     *            The message key
     * @param prerequisites
     *            if suffix for prerequisites has to be added
     * @param arguments
     *            The arguments to replace in message
     * @return The loaded property
     */
    protected CharSequence msg(final CharSequence key, final boolean prerequisites, final CharSequence... arguments) {
        if (prerequisites) {
            return getProperty(new StringBuilder(key).append(MSG.PRE), arguments);
        } else {
            this.notPersonalized = true;
            return getProperty(key, arguments);
        }
    }

    /**
     * @return the message
     */
    protected StringBuilder getMessage() {
        final StringBuilder message;
        if (this.previousPrerequisitesError) {
            message = this.previousMessage;
        } else if (this.prerequisitesError) {
            message = this.message;
        } else if (Boolean.FALSE.equals(this.previousValid) && !check(this.previousValid, this.previousCondition, this.valid)) {
            if (!this.valid) {
                message = new StringBuilder("(").append(this.previousMessage).append(")").append(OPERATORS[this.previousCondition])
                        .append("(").append(this.message).append(")");
            } else {
                message = this.previousMessage;
            }
        } else {
            message = this.message;
        }
        return message;
    }

    /**
     * @return the arguments
     */
    protected Object[] getParameters() {
        return this.parameters;
    }

    /**
     * Get the type of an object
     * 
     * @param object
     *            the object
     * @return The type or {@link TYPE#UNKNOWN}
     */
    protected static int getType(final Object object) {
        int type = TYPE.UNKNOWN;
        if (object != null) {
            final Class<?> clazz = object.getClass();
            if (Number.class.isAssignableFrom(clazz)) {
                if (NumberUtils.isNumberInteger(clazz)) {
                    type = TYPE.NUMBER_INTEGER;
                } else {
                    type = TYPE.NUMBER_DECIMAL;
                }
            } else if (CharSequence.class.isAssignableFrom(clazz)) {
                type = TYPE.CHAR_SEQUENCE;
            } else if (Boolean.class.isAssignableFrom(clazz)) {
                type = TYPE.BOOLEAN;
            } else if (clazz.isArray()) {
                type = TYPE.ARRAY;
            } else if (Iterable.class.isAssignableFrom(clazz)) {
                type = TYPE.ITERABLE;
            } else if (Map.class.isAssignableFrom(clazz)) {
                type = TYPE.MAP;
            } else if (Date.class.isAssignableFrom(clazz) || Calendar.class.isAssignableFrom(clazz)) {
                type = TYPE.DATE;
            } else if (Class.class.isInstance(object)) {
                type = TYPE.CLASS;
            }
        }
        return type;
    }

    /**
     * Build the next parameter string (example for the first one: %1$s*).
     * 
     * <pre>
     * Assertor.that(5).isEqual(3, "Invalid input for '%s'", CONST).or("text").isNotBlank().isOK();
     * // The value 5 will be the parameter 1 (%1$s*)
     * // The value 3 will be the parameter 2 (%2$s*)
     * // The value "text" will be the parameter 3 (%3$s*)
     * </pre>
     * 
     * @param addIndex
     *            the amount to add of the current one (equal to:
     *            <tt>parameters.length + addIndex</tt> with parameters from the
     *            first assertor)
     * @param type
     *            The index parameter type
     * @return the next parameter string
     */
    protected StringBuilder getNextParam(final int addIndex, final int type) {
        return getParam(this.parameters.length + addIndex, type);
    }

    /**
     * Build the next parameter string (example for the first one: %1$s*).
     * 
     * <pre>
     * Assertor.that(5).isEqual(3, "Invalid input for '%s'", CONST).or("text").isNotBlank().isOK();
     * // The value 5 will be the parameter 1 (%1$s*)
     * // The value 3 will be the parameter 2 (%2$s*)
     * // The value "text" will be the parameter 3 (%3$s*)
     * </pre>
     * 
     * @param addIndex
     *            the amount to add of the current one (equal to:
     *            <tt>parameters.length + addIndex</tt> with parameters from the
     *            first assertor)
     * @param object
     *            The object to get type
     * @return the next parameter string
     */
    protected StringBuilder getNextParam(final int addIndex, final Object object) {
        return getParam(this.parameters.length + addIndex, getType(object));
    }

    /**
     * @param index
     *            the index
     * @param type
     *            The index parameter type
     * @return the parameter string
     */
    protected static StringBuilder getParam(final int index, final int type) {
        final StringBuilder stringBuilder = new StringBuilder();
        final String percent = "%";
        if (TYPE.CHAR_SEQUENCE == type) {
            stringBuilder.append(percent).append(index).append("$s*");
        } else if (TYPE.BOOLEAN == type) {
            stringBuilder.append(percent).append(index).append("$B*");
        } else if (TYPE.NUMBER_INTEGER == type) {
            stringBuilder.append(percent).append(index).append("$,d*");
        } else if (TYPE.NUMBER_DECIMAL == type) {
            stringBuilder.append(percent).append(index).append("$,.3f*");
        } else if (TYPE.DATE == type) {
            stringBuilder.append(percent).append(index).append("$tY*/");
            stringBuilder.append(percent).append(index).append("$tm*/");
            stringBuilder.append(percent).append(index).append("$td* ");
            stringBuilder.append(percent).append(index).append("$tH*:");
            stringBuilder.append(percent).append(index).append("$tM*:");
            stringBuilder.append(percent).append(index).append("$tS* ");
            stringBuilder.append(percent).append(index).append("$tZ*");
        } else {
            stringBuilder.append(percent).append(index).append("$s*");
        }
        return stringBuilder;
    }

    /**
     * Combines the current boolean value with the new one
     * 
     * @param condition
     *            The condition between the two assertors
     * @param assertor
     *            The previous assertor
     * @return The operator
     */
    protected Operator<A, T> combine(final int condition, final A assertor) {
        if (this.not) {
            throw new IllegalArgumentException("'Not' cannot be followed by a condition");
        }
        // store the previous data (easy access)
        this.previousCondition = condition;
        this.previousValid = assertor.isValid();
        this.previousPrerequisitesError = assertor.isPrerequisitesError();
        this.previousAssertor = assertor;
        this.previousMessage = assertor.getMessage();

        // if already in error (no combine has to proceed from this point)
        this.prerequisitesError = this.previousPrerequisitesError;

        // override parameters to keep order
        this.parameters = ArrayUtils.addAll(assertor.getParameters(), this.parameters);

        // prepare variables (for easy access)
        this.paramIndex = this.parameters.length;
        this.param = getParam(this.paramIndex, this.type);

        return this.getOperator();
    }

    /**
     * Combines the current boolean value with the new one
     * 
     * @param prerequisite
     *            The prior condition
     * @param supplier
     *            The specific check (if prerequisite is true)
     * @param preMessage
     *            The message if prior condition aren't valid (as supplier)
     * @param message
     *            The message on error
     * @param arguments
     *            The message arguments
     * @param locale
     *            The locale for the message, use {@link String#format}
     * @param parameters
     *            The parameters
     * @param <E>
     *            The type of supplier exception
     * @return The operator
     */
    protected <E extends Throwable> Operator<A, T> combine(final boolean prerequisite, final BooleanSupplierThrowable<E> supplier,
            final Supplier<CharSequence> preMessage, final CharSequence message, final Object[] arguments, final Locale locale,
            final Object... parameters) {
        if (message != null && (locale != null || ArrayUtils.isNotEmpty(arguments))) {
            this.parameters = ArrayUtils.addAll(this.parameters, parameters);
            return this.combine(prerequisite, supplier, preMessage,
                    EndPoints.getMessage(DEFAULT_ASSERTION, locale, message, this.parameters, arguments));
        } else {
            return this.combine(prerequisite, supplier, preMessage, message, parameters);
        }
    }

    /**
     * Combines the current condition value with the new one. Check for
     * preconditions, if they are not valid, the message (and previous) is
     * replaced by the prerequisites message.
     * 
     * @param prerequisite
     *            The prior condition
     * @param supplier
     *            The specific check (if prerequisite is true)
     * @param preMessage
     *            The message if prior condition aren't valid (as supplier)
     * @param message
     *            The message on error
     * @param parameters
     *            The parameters
     * @param <E>
     *            The type of supplier exception
     * @return The operator
     */
    protected <E extends Throwable> Operator<A, T> combine(final boolean prerequisite, final BooleanSupplierThrowable<E> supplier,
            final Supplier<CharSequence> preMessage, final CharSequence message, final Object... parameters) {
        boolean c = false;

        if (!this.prerequisitesError) {
            if (prerequisite) {
                if (supplier != null) {
                    c = this.not ^ supplier.getAsBoolean();
                } else {
                    c = !this.not;
                }

                this.valid = check(this.valid, this.condition, c);

                if (!this.valid && StringUtils.isNotEmpty(message)) {
                    if (this.message.length() > 0) {
                        this.message.append(OPERATORS[this.condition]);
                    }
                    if (this.not && this.notPersonalized) {
                        this.message.append(NOT).append(message).append(EnumChar.PARENTHESIS_CLOSE);
                    } else {
                        this.message.append(message);
                    }
                } else if (this.valid) {
                    this.message = new StringBuilder();
                }

                this.parameters = ArrayUtils.addAll(this.parameters, parameters);

                this.notPersonalized = false;
                this.not = false;
            } else {
                if (preMessage != null) {
                    this.message = new StringBuilder(preMessage.get());
                } else {
                    this.message = new StringBuilder(DEFAULT_ASSERTION);
                }

                this.prerequisitesError = true;
                this.valid = false;
            }
        }

        return this.getOperator();
    }

    private static boolean check(final boolean first, final int condition, final boolean second) {
        boolean valid = first;
        switch (condition) {
        case AND:
            valid &= second;
            break;
        case OR:
            valid |= second;
            break;
        case XOR:
            valid ^= second;
            break;
        default:
        }
        return valid;
    }

    /**
     * Apply a not on the next condition. Be aware, some functions check for
     * {@code null} by example and the result will not as expected.
     * 
     * <pre>
     * Assertor.that(object).not().isNull().toThrow();
     * // Same expression
     * Assertor.that(object).isNotNull().toThrow();
     * 
     * // Other example (same result)
     * Assertor.that("text").not().hasLength(3).toThrow(); // true
     * Assertor.that("text").hasNotLength(3).toThrow(); // true
     * 
     * // But here, it's different (both methods check for null and length not
     * // lower to zero)
     * Assertor.that("text").not().hasLength(-1).toThrow(); // true
     * Assertor.that("text").hasNotLength(-1).toThrow(); // false
     * </pre>
     * 
     * @return This
     */
    public A not() {
        this.not = !this.not;
        return this.getThis();
    }

    /**
     * @return true if NOT operator is active
     */
    protected boolean isNot() {
        return this.not;
    }
}