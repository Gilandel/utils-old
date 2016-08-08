/*-
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.tuple.Triple;

import fr.landel.utils.commons.EnumChar;

/**
 * Class to store results of assertion at each step. It contains the current
 * result and the list of parameters added at each assertion.
 * 
 * <p>
 * What it's stored:
 * </p>
 * <ul>
 * <li>object: the object to check</li>
 * <li>type: the type of object to check</li>
 * <li>operator: the operator applied to the next step</li>
 * <li>not: if not has to be applied to the next step</li>
 * <li>preconditionOK: if preconditions are valid (here we ensured that the
 * check will not throw a {@link NullPointerException} by example)</li>
 * <li>valid: the result of the check (the associated function is only run if
 * preconditions are ok)</li>
 * <li>preconditionMessage: the message applied if preconditions aren't
 * valid</li>
 * <li>message: the message applied if preconditions are valid and check result
 * is ko</li>
 * <li>parameters: the whole list of parameters (includes previous checked
 * object and assertion parameters)</li>
 * </ul>
 *
 * @since Aug 7, 2016
 * @author Gilles
 *
 * @param <T>
 *            the type of assertor result
 */
public class AssertorResult<T> implements Serializable {

    /**
     * serialVersionUID
     */
    private static final long serialVersionUID = -8316517833558101416L;

    private final T object;
    private final EnumType type;
    private final EnumOperator operator;
    private final boolean not;
    private final boolean preconditionOK;
    private final boolean valid;
    private CharSequence preconditionMessage;
    private CharSequence message;

    private List<Triple<Object, EnumType, Boolean>> parameters;

    /**
     * 
     * Base constructor
     *
     * @param object
     *            the object under check
     * @param parameters
     *            all the parameters including checked objects
     * @param preconditionOK
     *            if prerequisites are OK
     * @param valid
     *            if valid (predication must be true)
     * @param operator
     *            the operator for the next combination
     * @param not
     *            if NOT operator is applied on the next checker
     * @param preconditionMessage
     *            the message (only applied if precondition is KO)
     * @param message
     *            the message (only applied if precondition is OK and checker is
     *            KO)
     */
    private AssertorResult(final T object, final EnumType type, final List<Triple<Object, EnumType, Boolean>> parameters,
            final boolean preconditionOK, final boolean valid, final EnumOperator operator, final boolean not,
            final CharSequence preconditionMessage, final CharSequence message) {

        this.object = object;
        this.type = type;
        this.preconditionOK = preconditionOK;
        this.valid = valid;
        this.operator = operator;
        this.not = not;

        if (preconditionMessage != null) {
            this.preconditionMessage = preconditionMessage;
        } else {
            this.preconditionMessage = "";
        }

        if (message != null) {
            this.message = message;
        } else {
            this.message = "";
        }

        if (parameters != null) {
            this.parameters = new ArrayList<>(parameters);
        } else {
            this.parameters = new ArrayList<>();
        }
    }

    /**
     * 
     * Constructor for each {@link Assertor#that}
     *
     * @param object
     *            the object under check
     * @param type
     *            the type of the object
     */
    public AssertorResult(final T object, final EnumType type) {
        this(object, type, null, true, true, null, false, null, null);

        this.addObject(object, type);
    }

    /**
     * 
     * Combining constructor, for (NOT operator) (not is set to true)
     *
     * @param result
     *            the previous result
     */
    public AssertorResult(final AssertorResult<T> result) {
        this(result.getObject(), result.getType(), result.getParameters(), result.isPreconditionOK(), result.isValid(),
                result.getOperator(), !result.isNot(), result.getPreconditionMessage(), result.getMessage());
    }

    /**
     * 
     * Combining constructor with new object, for (AND, OR and XOR combinations)
     *
     * @param result
     *            the previous result
     * @param object
     *            the object under check
     * @param operator
     *            the operator for the next combination
     * @param <X>
     *            the type of other assertor result
     */
    public <X> AssertorResult(final AssertorResult<X> result, final T object, final EnumOperator operator) {
        this(object, EnumType.getType(object), result.getParameters(), result.isPreconditionOK(), result.isValid(), operator, false,
                result.getPreconditionMessage(), result.getMessage());

        this.addObject(object, this.type);
    }

    /**
     * 
     * Combining constructor, for (AND, OR and XOR combinations)
     *
     * @param result
     *            The previous result
     * @param operator
     *            The operator for the next combination
     */
    public AssertorResult(final AssertorResult<T> result, final EnumOperator operator) {
        this(result.getObject(), result.getType(), result.getParameters(), result.isPreconditionOK(), result.isValid(), operator, false,
                result.getPreconditionMessage(), result.getMessage());
    }

    /**
     * 
     * Combining constructor, for sub assertor (AND, OR and XOR combinations)
     *
     * @param result
     *            The previous result
     * @param other
     *            The sub result
     * @param operator
     *            The operator for the next combination
     * @param <X>
     *            the type of the other assertor result
     */
    public <X> AssertorResult(final AssertorResult<T> result, final AssertorResult<X> other, final EnumOperator operator) {
        this(result.getObject(), result.getType(), result.getParameters(), result.isPreconditionOK() && other.isPreconditionOK(),
                AssertorHelper.isValid(result.isValid(), other.isValid(), operator), operator, false, null, null);

        result.getParameters().addAll(other.getParameters());

        this.preconditionMessage = getPreconditionMessage(result.isPreconditionOK(), other.isPreconditionOK(),
                result.getPreconditionMessage(), other.getPreconditionMessage());
        this.message = getMessage(this.isValid(), result.isValid(), other.isValid(), result.getMessage(), other.getMessage(), operator);
    }

    /**
     * 
     * Standard combining constructor (not is set to false)
     *
     * @param result
     *            The previous result
     * @param preconditionOK
     *            If prerequisites are OK
     * @param valid
     *            If valid (predication must be true)
     * @param preconditionMessage
     *            The message (only applied if precondition is KO)
     * @param message
     *            The message (only applied if precondition and checker are OK)
     */
    public AssertorResult(final AssertorResult<T> result, final boolean preconditionOK, final boolean valid,
            final CharSequence preconditionMessage, final CharSequence message) {
        this(result.getObject(), result.getType(), result.getParameters(), preconditionOK, valid, result.getOperator(), false,
                preconditionMessage, message);
    }

    private void addObject(final T object, final EnumType type) {
        this.getParameters().add(Triple.of(object, type, true));
    }

    private static CharSequence getPreconditionMessage(final boolean previousPreconditionOK, final boolean preconditionOK,
            final CharSequence previousPreconditionMessage, final CharSequence preconditionMessage) {
        final StringBuilder message = new StringBuilder();

        if (!previousPreconditionOK && !preconditionOK) {
            message.append(previousPreconditionMessage).append(EnumOperator.AND).append(preconditionMessage);
        } else if (!previousPreconditionOK) {
            message.append(previousPreconditionMessage);
        } else if (!preconditionOK) {
            message.append(preconditionMessage);
        }

        return message;
    }

    private static <T> CharSequence getMessage(final boolean OK, final boolean previousOK, final boolean currentOK,
            final CharSequence previousMessage, final CharSequence message, final EnumOperator operator) {
        final StringBuilder sb = new StringBuilder();
        if (!OK) {
            if (operator != null && !previousOK) {
                sb.append(previousMessage);
                if (!currentOK) {
                    sb.append(operator);
                }
            }
            if (!currentOK && message != null) {
                sb.append(EnumChar.PARENTHESIS_OPEN).append(message).append(EnumChar.PARENTHESIS_CLOSE);
            }
        }
        return sb;
    }

    /**
     * @return the object
     */
    public T getObject() {
        return this.object;
    }

    /**
     * @return the type
     */
    protected EnumType getType() {
        return this.type;
    }

    /**
     * @return if precondition is ok
     */
    public boolean isPreconditionOK() {
        return this.preconditionOK;
    }

    /**
     * @return if valid
     */
    public boolean isValid() {
        return this.valid;
    }

    /**
     * @return the messages supplier
     */
    public CharSequence getMessage() {
        return this.message;
    }

    /**
     * @return the preconditionMessage
     */
    protected CharSequence getPreconditionMessage() {
        return this.preconditionMessage;
    }

    /**
     * @return the operator
     */
    protected EnumOperator getOperator() {
        return this.operator;
    }

    /**
     * @return the not
     */
    protected boolean isNot() {
        return this.not;
    }

    /**
     * @return the parameters
     */
    protected List<Triple<Object, EnumType, Boolean>> getParameters() {
        return this.parameters;
    }
}