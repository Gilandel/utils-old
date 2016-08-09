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

import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.collections4.Transformer;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import fr.landel.utils.commons.CollectionUtils2;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

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

    private static final Transformer<Pair<Object, EnumType>, Triple<Object, EnumType, Boolean>> PAIR_TO_TRIPLE_TRANSFORMER = new Transformer<Pair<Object, EnumType>, Triple<Object, EnumType, Boolean>>() {
        @Override
        public Triple<Object, EnumType, Boolean> transform(final Pair<Object, EnumType> input) {
            return Triple.of(input.getLeft(), input.getValue(), false);
        }
    };

    protected static <T> AssertorResult<T> not(final AssertorResult<T> result) {
        return new AssertorResult<>(result);
    }

    protected static <X, T> AssertorResult<T> and(final AssertorResult<X> result, final T object) {
        return new AssertorResult<>(result, object, EnumOperator.AND);
    }

    protected static <X, T> AssertorResult<T> or(final AssertorResult<X> result, final T object) {
        return new AssertorResult<>(result, object, EnumOperator.OR);
    }

    protected static <X, T> AssertorResult<T> xor(final AssertorResult<X> result, final T object) {
        return new AssertorResult<>(result, object, EnumOperator.XOR);
    }

    protected static <T> AssertorResult<T> and(final AssertorResult<T> result) {
        return new AssertorResult<>(result, EnumOperator.AND);
    }

    protected static <T> AssertorResult<T> or(final AssertorResult<T> result) {
        return new AssertorResult<>(result, EnumOperator.OR);
    }

    protected static <T> AssertorResult<T> xor(final AssertorResult<T> result) {
        return new AssertorResult<>(result, EnumOperator.XOR);
    }

    protected static <T, X> AssertorResult<T> and(final AssertorResult<T> result, final AssertorResult<X> other) {
        return new AssertorResult<>(result, other, EnumOperator.AND);
    }

    protected static <T, X> AssertorResult<T> or(final AssertorResult<T> result, final AssertorResult<X> other) {
        return new AssertorResult<>(result, other, EnumOperator.OR);
    }

    protected static <T, X> AssertorResult<T> xor(final AssertorResult<T> result, final AssertorResult<X> other) {
        return new AssertorResult<>(result, other, EnumOperator.XOR);
    }

    @SafeVarargs
    protected static <T, E extends Throwable> AssertorResult<T> combine(final AssertorResult<T> result,
            final Function<T, Boolean> precondition, final BiFunctionThrowable<T, Boolean, Boolean, E> checker,
            final BiFunction<Integer, Integer, CharSequence> preconditionMessage,
            final TriFunction<Integer, Integer, Boolean, CharSequence> message, final boolean notAppliedByChecker,
            final Pair<Object, EnumType>... parameters) {

        int objectIndex = 0;
        final int paramSize = result.getParameters().size();
        for (int i = paramSize - 1; i >= 0; i--) {
            if (result.getParameters().get(i).getRight()) {
                objectIndex = i;
                break;
            }
        }

        if (parameters.length > 0) {
            result.getParameters().addAll(CollectionUtils2.transformIntoList(parameters, PAIR_TO_TRIPLE_TRANSFORMER));
        }

        final boolean previousPrecondition = result.isPreconditionOK();
        final boolean currentPrecondition = precondition == null || precondition.apply(result.getObject());

        final boolean newPrecondition;
        final boolean newValid;
        final CharSequence newPreconditionMessage;
        final CharSequence newMessage;

        if (!previousPrecondition || !currentPrecondition) {
            newPrecondition = false;
            newPreconditionMessage = getPreconditionErrors(result, previousPrecondition, currentPrecondition, preconditionMessage,
                    objectIndex, paramSize);
            newValid = false;
            newMessage = EMPTY_STRING;
        } else {
            final boolean previousOK = result.isValid();
            final boolean currentOK = callChecker(result, checker, notAppliedByChecker);

            newPrecondition = true;
            newPreconditionMessage = EMPTY_STRING;
            newValid = isValid(previousOK, currentOK, result.getOperator());
            newMessage = getErrors(newValid, previousOK, currentOK, result, message, objectIndex, paramSize);
        }

        return new AssertorResult<>(result, newPrecondition, newValid, newPreconditionMessage, newMessage);
    }

    private static <T, E extends Throwable> boolean callChecker(final AssertorResult<T> result,
            final BiFunctionThrowable<T, Boolean, Boolean, E> checker, final boolean notAppliedByChecker) {
        boolean currentOK;
        if (checker != null) {
            try {
                if (notAppliedByChecker) {
                    currentOK = checker.apply(result.getObject(), result.isNot());
                } else {
                    currentOK = result.isNot() ^ checker.apply(result.getObject(), result.isNot());
                }
            } catch (Throwable e) {
                currentOK = result.isNot() ^ false;
            }
        } else {
            currentOK = !result.isNot();
        }
        return currentOK;
    }

    private static <T> CharSequence getPreconditionErrors(final AssertorResult<T> result, final boolean previousPrecondition,
            final boolean currentPrecondition, final BiFunction<Integer, Integer, CharSequence> preconditionMessage, final int objectIndex,
            final int paramSize) {
        final StringBuilder sb = new StringBuilder();
        if (!previousPrecondition && !currentPrecondition) {
            sb.append(result.getPreconditionMessage()).append(EnumOperator.AND).append(preconditionMessage.apply(objectIndex, paramSize));
        } else if (!previousPrecondition) {
            sb.append(result.getPreconditionMessage());
        } else {
            sb.append(preconditionMessage.apply(objectIndex, paramSize));
        }
        return sb;
    }

    private static <T> CharSequence getErrors(final boolean OK, final boolean previousOK, final boolean currentOK,
            final AssertorResult<T> result, final TriFunction<Integer, Integer, Boolean, CharSequence> message, final int objectIndex,
            final int paramSize) {
        final StringBuilder sb = new StringBuilder();
        if (!OK) {
            if (result.getOperator() != null && !previousOK) {
                sb.append(result.getMessage());
                if (!currentOK) {
                    sb.append(result.getOperator());
                }
            }
            if (!currentOK && message != null) {
                sb.append(message.apply(objectIndex, paramSize, result.isNot()));
            }
        }
        return sb;
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
