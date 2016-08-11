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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.collections4.Transformer;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import fr.landel.utils.commons.EnumChar;

/**
 * 
 * Assertor helper class, to build exceptions and messages.
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public class HelperAssertor extends Constants {

    /**
     * Empty {@code String}
     */
    protected static final CharSequence EMPTY_STRING = "";

    protected static final Transformer<Pair<Object, EnumType>, Object> PARAM_TRANSFORMER = new Transformer<Pair<Object, EnumType>, Object>() {
        @Override
        public Object transform(final Pair<Object, EnumType> input) {
            return input.getKey();
        }
    };

    protected static <T> StepAssertor<T> not(final StepAssertor<T> result) {
        return new StepAssertor<>(result);
    }

    protected static <X, T> StepAssertor<T> and(final StepAssertor<X> result, final T object, final EnumType type) {
        return new StepAssertor<>(result, object, type, EnumOperator.AND);
    }

    protected static <X, T> StepAssertor<T> or(final StepAssertor<X> result, final T object, final EnumType type) {
        return new StepAssertor<>(result, object, type, EnumOperator.OR);
    }

    protected static <X, T> StepAssertor<T> xor(final StepAssertor<X> result, final T object, final EnumType type) {
        return new StepAssertor<>(result, object, type, EnumOperator.XOR);
    }

    protected static <T> StepAssertor<T> and(final StepAssertor<T> result) {
        return new StepAssertor<>(result, EnumOperator.AND);
    }

    protected static <T> StepAssertor<T> or(final StepAssertor<T> result) {
        return new StepAssertor<>(result, EnumOperator.OR);
    }

    protected static <T> StepAssertor<T> xor(final StepAssertor<T> result) {
        return new StepAssertor<>(result, EnumOperator.XOR);
    }

    protected static <T, X> StepAssertor<T> and(final StepAssertor<T> result, final StepAssertor<X> other) {
        return new StepAssertor<>(result, other, EnumOperator.AND);
    }

    protected static <T, X> StepAssertor<T> or(final StepAssertor<T> result, final StepAssertor<X> other) {
        return new StepAssertor<>(result, other, EnumOperator.OR);
    }

    protected static <T, X> StepAssertor<T> xor(final StepAssertor<T> result, final StepAssertor<X> other) {
        return new StepAssertor<>(result, other, EnumOperator.XOR);
    }

    protected static <T> ResultAssertor combine(final StepAssertor<T> step, final boolean loadMessage) {

        // load all steps and reverse the order
        final List<StepAssertor<?>> steps = new ArrayList<>();
        steps.add(step);
        StepAssertor<?> currentStep = step;
        StepAssertor<?> previousStep;
        while ((previousStep = currentStep.getPreviousStep()) != null) {
            steps.add(previousStep);
            currentStep = previousStep;

        }
        Collections.reverse(steps);

        boolean not = false;
        boolean valid = true;
        EnumOperator operator = null;
        final StringBuilder message = new StringBuilder();
        Object object = null;
        EnumType type = null;
        Pair<Object, EnumType> pair = null;

        final List<Pair<Object, EnumType>> parameters = new ArrayList<>();

        for (StepAssertor<?> s : steps) {
            if (EnumStep.CREATION.equals(s.getStepType())) {

                object = s.getObject();
                type = s.getType();
                pair = Pair.of(object, type);

                parameters.add(pair);

            } else if (EnumStep.ASSERTION.equals(s.getStepType())) {

                parameters.addAll(s.getParameters());

                if (!EnumOperator.AND.equals(operator) || valid) {
                    // if precondition returns false, we end all treatments
                    if (!HelperAssertor.preCheck(s, object)) {
                        return HelperAssertor.getPreconditionMessage(s, pair, parameters, loadMessage);

                    } else {
                        valid = HelperAssertor.validatesAndGetMessage(s, pair, object, valid, not, operator, message, loadMessage);
                    }
                }

                not = false;

            } else if (EnumStep.OPERATOR.equals(s.getStepType())) {

                operator = s.getOperator();

            } else if (EnumStep.NOT.equals(s.getStepType())) {

                not = not ^ s.isNot();

            } else if (EnumStep.OBJECT.equals(s.getStepType())) {

                operator = s.getOperator();
                object = s.getObject();
                type = s.getType();
                pair = Pair.of(object, type);

                parameters.add(pair);

            } else if (EnumStep.SUB.equals(s.getStepType()) && s.getSubStep() != null) {
                final Triple<Boolean, EnumOperator, ResultAssertor> output = HelperAssertor.managesSub(s, parameters, valid, operator,
                        message, loadMessage);

                if (output.getRight() != null) {
                    return output.getRight();
                } else {
                    valid = output.getLeft();
                    operator = output.getMiddle();
                }
            }
        }

        return new ResultAssertor(true, valid, message.toString(), parameters);
    }

    private static <T> ResultAssertor getPreconditionMessage(final StepAssertor<T> step, final Pair<Object, EnumType> pair,
            final List<Pair<Object, EnumType>> parameters, final boolean loadMessage) {

        final List<Pair<Object, EnumType>> assertParameters = new ArrayList<>();
        assertParameters.add(pair);
        assertParameters.addAll(step.getParameters());

        final String error;
        if (loadMessage) {
            final String preconditionMessage = HelperMessage.getDefaultMessage(step.getMessageKey(), true, false, assertParameters);
            error = String.format(Assertor.getLocale(), preconditionMessage, assertParameters);
        } else {
            error = null;
        }

        return new ResultAssertor(false, false, error, parameters);
    }

    private static <T> boolean validatesAndGetMessage(final StepAssertor<T> step, final Pair<Object, EnumType> pair, final Object object,
            final boolean valid, final boolean not, final EnumOperator operator, final StringBuilder message, final boolean loadMessage) {

        boolean nextValid = HelperAssertor.isValid(valid, HelperAssertor.check(step, object, not), operator);

        if (!nextValid && loadMessage) {
            if (message.length() > 0 && operator != null) {
                message.append(operator);
            }

            final List<Pair<Object, EnumType>> assertParameters = new ArrayList<>();
            assertParameters.add(pair);
            assertParameters.addAll(step.getParameters());

            message.append(
                    HelperMessage.getMessage(step.getMessage(), step.getMessageKey(), not ^ step.isMessageKeyNot(), assertParameters));

        }

        return nextValid;
    }

    private static <T> Triple<Boolean, EnumOperator, ResultAssertor> managesSub(final StepAssertor<T> step,
            final List<Pair<Object, EnumType>> parameters, final boolean valid, final EnumOperator operator, final StringBuilder message,
            final boolean loadMessage) {

        final StepAssertor<?> subStep = step.getSubStep();
        EnumOperator nextOperator = operator;
        boolean nextValid = valid;

        if (!EnumOperator.AND.equals(step.getOperator()) || nextValid) {

            final ResultAssertor subResult = HelperAssertor.combine(subStep, loadMessage);

            if (!subResult.isPrecondition()) {
                return Triple.of(false, null, subResult);
            } else {
                nextOperator = step.getOperator();

                nextValid = HelperAssertor.isValid(nextValid, subResult.isValid(), nextOperator);

                parameters.addAll(subResult.getParameters());

                if (!nextValid && loadMessage && subResult.getMessage() != null) {

                    if (message.length() > 0 && nextOperator != null) {
                        message.append(nextOperator);
                    }

                    message.append(EnumChar.PARENTHESIS_OPEN);
                    message.append(subResult.getMessage());
                    message.append(EnumChar.PARENTHESIS_CLOSE);
                }
            }
        }

        return Triple.of(nextValid, nextOperator, null);
    }

    @SuppressWarnings("unchecked")
    private static <T> boolean preCheck(final StepAssertor<T> step, final Object object) {
        if (step.getPreChecker() != null) {
            return step.getPreChecker().test((T) object);
        }
        return true;
    }

    @SuppressWarnings("unchecked")
    private static <T> boolean check(final StepAssertor<T> step, final Object object, final boolean not) {
        if (step.getChecker() != null) {
            try {
                if (step.isNotAppliedByChecker()) {
                    return step.getChecker().test((T) object, not);
                } else {
                    return not ^ step.getChecker().test((T) object, not);
                }
            } catch (Throwable e) {
                return false;
            }
        }
        return !not;
    }

    protected static boolean isValid(final boolean previousOK, final boolean currentOK, final EnumOperator operator) {
        boolean OK = false;
        if (EnumOperator.AND.equals(operator)) {
            OK = previousOK & currentOK;
        } else if (EnumOperator.OR.equals(operator)) {
            OK = previousOK | currentOK;
        } else if (EnumOperator.XOR.equals(operator)) {
            OK = previousOK ^ currentOK;
        } else {
            OK = previousOK & currentOK;
        }
        return OK;
    }

    protected static boolean isValid(final boolean all, final boolean not, final int found, final int size) {
        if (all) {
            if (not) { // NOT ALL
                return found > 0 && found < size;
            } else { // ALL
                return found == size;
            }
        } else if (not) { // NOT ANY
            return found == 0;
        } else { // ANY
            return found > 0;
        }
    }
}
