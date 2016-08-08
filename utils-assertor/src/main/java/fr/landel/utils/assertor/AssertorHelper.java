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

import java.util.Calendar;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.Transformer;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.lang3.tuple.Triple;

import fr.landel.utils.commons.CollectionUtils2;
import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

/**
 * 
 * Assertor helper class, to build exceptions and messages.
 *
 * @since 3 août 2016
 * @author Gilles
 *
 */
public final class AssertorHelper extends AssertorConstants {

    /**
     * Empty {@code String}
     */
    protected static final CharSequence EMPTY_STRING = "";

    private static final Transformer<Triple<Object, EnumType, Boolean>, Object> PARAM_TRANSFORMER = new Transformer<Triple<Object, EnumType, Boolean>, Object>() {
        @Override
        public Object transform(final Triple<Object, EnumType, Boolean> input) {
            return input.getLeft();
        }
    };

    private static final Transformer<Pair<Object, EnumType>, Triple<Object, EnumType, Boolean>> PAIR_TO_TRIPLE_TRANSFORMER = new Transformer<Pair<Object, EnumType>, Triple<Object, EnumType, Boolean>>() {
        @Override
        public Triple<Object, EnumType, Boolean> transform(final Pair<Object, EnumType> input) {
            return Triple.of(input.getLeft(), input.getValue(), false);
        }
    };

    /**
     * {@link java.util.Formatter}
     * 
     * %[argument_index$][flags][width][.precision][t]conversion
     */
    private static final String FORMAT_SPECIFIER = "%(\\d+\\$)?([-#+ 0,(\\<]*)?(\\d+)?(\\.\\d+)?([tT])?([a-zA-Z%])";
    private static final Pattern PATTERN = Pattern.compile(FORMAT_SPECIFIER);
    private static final String PARAM_ID = "*";

    /**
     * Hidden Constructor
     */
    private AssertorHelper() {
        super();
    }

    /**
     * Gets the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %s* or %1$s*
     * 
     * <pre>
     * Operator.getMessage(10, 20, &quot;The number '%s*' is not greater than number '%s*'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * Operator.getMessage(10, 20, &quot;'%2$s*' &gt; '%1$s*'&quot;);
     * // Exception: "'20' &gt; '10'"
     * </pre>
     * 
     * @param result
     *            The assertor result
     * @param locale
     *            The message locale
     * @param message
     *            The user message
     * @param arguments
     *            The user arguments
     * @param defaultKey
     *            The default message key
     * @param not
     *            If not has to be appended to the default message key
     * @param paramIndexes
     *            The list of parameters to inject in message
     * @param <T>
     *            the type assertor result
     * @return The message formatted
     */
    protected static <T> String getMessage(final AssertorResult<T> result, final Locale locale, final CharSequence message,
            final Object[] arguments, final CharSequence defaultKey, final boolean not, final Integer... paramIndexes) {

        final CharSequence currentMessage;
        final Object[] currentArguments;
        if (message != null) {
            currentMessage = message;
            currentArguments = arguments;
        } else {
            currentMessage = AssertorHelper.msg(result, defaultKey, false, not, paramIndexes);
            currentArguments = null;
        }

        return AssertorHelper.getMessage(AssertorConstants.DEFAULT_ASSERTION, locale, currentMessage, result.getParameters(),
                currentArguments);
    }

    /**
     * Gets the message (the locale can be change through <code>setLocale</code>
     * ). Supports injecting parameters in message by using %s* or %1$s*
     * 
     * <pre>
     * Operator.getMessage(10, 20, &quot;The number '%s*' is not greater than number '%s*'&quot;);
     * // Exception: "The number '10' is not greater than number '20'"
     * Operator.getMessage(10, 20, &quot;'%2$s*' &gt; '%1$s*'&quot;);
     * // Exception: "'20' &gt; '10'"
     * </pre>
     * 
     * @param defaultString
     *            The default message provided by each method
     * @param locale
     *            The message locale
     * @param message
     *            The user message
     * @param parameters
     *            The method parameters
     * @param arguments
     *            The user arguments
     * @return The message formatted
     */
    public static String getMessage(final CharSequence defaultString, final Locale locale, final CharSequence message,
            final List<Triple<Object, EnumType, Boolean>> parameters, final Object[] arguments) {

        String msg;
        Locale l = Assertor.getLocale(locale);

        if (StringUtils.isNotEmpty(message)) {
            msg = message.toString();
            if (CollectionUtils.isNotEmpty(parameters)) {
                msg = AssertorHelper.formatParameters(l, msg, parameters);
            }
            if (ArrayUtils.isNotEmpty(arguments)) {
                msg = String.format(ObjectUtils.defaultIfNull(l, Assertor.getLocale()), msg, arguments);
            }
        } else {
            msg = defaultString.toString();
        }
        return msg;
    }

    /**
     * Formats the parameter expressions without modification of standard
     * expressions
     * 
     * @param message
     *            The message where to replace the parameters
     * @param parameters
     *            The parameter values (ordered)
     * @return The formatted result
     */
    private static String formatParameters(final Locale locale, final String message,
            final List<Triple<Object, EnumType, Boolean>> parameters) {

        // detected character by {@link String#format}
        final String percent = "%";
        final String replacement = "\u00b6\u00b6\u00b6";

        final int paramIdLength = PARAM_ID.length();
        final int length = message.length();

        final StringBuilder newMessage = new StringBuilder();
        final java.util.regex.Matcher matcher = PATTERN.matcher(message);

        int start;
        int end;
        int previousEnd = 0;
        int move = 0;

        while (matcher.find()) {
            start = matcher.start();
            end = matcher.end();
            if (start - previousEnd - move > 0) {
                newMessage.append(message.substring(previousEnd + move, start));
            }
            if (end + paramIdLength > length || !PARAM_ID.equals(message.substring(end, end + paramIdLength))) {
                newMessage.append(StringUtils.replace(matcher.group(0), percent, replacement));
                move = 0;
            } else {
                newMessage.append(matcher.group(0));
                move = paramIdLength;
            }
            previousEnd = end;
        }

        if (previousEnd + move < length) {
            newMessage.append(message.substring(previousEnd + move, length));
        }

        return StringUtils.replace(String.format(locale, newMessage.toString(), convertParams(parameters)), replacement, percent);
    }

    private static Object[] convertParams(final List<Triple<Object, EnumType, Boolean>> parameters) {
        List<Object> convertedParams = CollectionUtils2.transformIntoList(parameters, PARAM_TRANSFORMER);
        // The object, the type and if it's a checked object
        Triple<Object, EnumType, Boolean> triple;
        int calendarField = -1;

        for (int i = 0; i < parameters.size(); i++) {
            triple = parameters.get(i);
            if (triple.getLeft() != null) {
                if (EnumType.DATE.equals(triple.getMiddle()) && Calendar.class.isAssignableFrom(triple.getLeft().getClass())) {
                    convertedParams.set(i, ((Calendar) triple.getLeft()).getTime());
                } else if (EnumType.CALENDAR_FIELD.equals(triple.getMiddle())) {
                    calendarField = (Integer) triple.getLeft();
                    if (CALENDAR_FIELDS.containsKey(calendarField)) {
                        convertedParams.set(i, CALENDAR_FIELDS.get(calendarField));
                    }
                } else if (EnumType.CLASS.equals(triple.getMiddle())) {
                    convertedParams.set(i, ((Class<?>) triple.getLeft()).getSimpleName());
                }
            }
        }
        return convertedParams.toArray();
    }

    /**
     * Generates parameter string with default format for each supported types
     * 
     * @param index
     *            The index
     * @param type
     *            The index parameter type
     * @return the parameter string
     */
    protected static StringBuilder getParam(final int index, final EnumType type) {
        final StringBuilder stringBuilder = new StringBuilder();
        final String percent = "%";
        if (EnumType.CHAR_SEQUENCE.equals(type)) {
            stringBuilder.append(percent).append(index).append("$s*");
        } else if (EnumType.BOOLEAN.equals(type)) {
            stringBuilder.append(percent).append(index).append("$B*");
        } else if (EnumType.NUMBER_INTEGER.equals(type)) {
            stringBuilder.append(percent).append(index).append("$,d*");
        } else if (EnumType.NUMBER_DECIMAL.equals(type)) {
            stringBuilder.append(percent).append(index).append("$,.3f*");
        } else if (EnumType.DATE.equals(type)) {
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
     * Get the message and define that the current condition uses a personalized
     * message, not the default one
     * 
     * @param result
     *            The assertor result (required, not null)
     * @param key
     *            The message key (required, not null)
     * @param precondition
     *            If 'precondition' suffix has to be appended
     * @param not
     *            If 'not' suffix has to be appended
     * @param paramIndexes
     *            The arguments index to replace in message
     * @param <T>
     *            the type of assertor result
     * @return The loaded property
     */
    protected static <T> CharSequence msg(final AssertorResult<T> result, final CharSequence key, final boolean precondition,
            final boolean not, final Integer... paramIndexes) {
        Objects.requireNonNull(result);
        Objects.requireNonNull(key);

        final StringBuilder keyProperty = new StringBuilder(key);

        if (precondition) {
            keyProperty.append(MSG.PRE);
        } else if (not) {
            // NOT is ignored if precondition mode
            // precondition is the same with or without not
            keyProperty.append(MSG.NOT);
        }

        final CharSequence[] arguments = new CharSequence[paramIndexes.length];
        for (int i = 0; i < paramIndexes.length; i++) {
            arguments[i] = AssertorHelper.getParam(paramIndexes[i] + 1, result.getParameters().get(paramIndexes[i]).getMiddle());
        }

        return getProperty(keyProperty, arguments);
    }

    @SuppressWarnings("unchecked")
    protected static <S extends PredicateStep<S, T>, T> S cast(final PredicateStep<S, T> predicate) {
        return (S) predicate;
    }

    @SuppressWarnings("unchecked")
    protected static <S extends PredicateStep<S, T>, P extends PredicateAssertor<S, T>, T> P cast(final PredicateAssertor<S, T> predicate) {
        return (P) predicate;
    }

    protected static <T> Supplier<AssertorResult<T>> not(final Supplier<AssertorResult<T>> supplier) {
        return () -> new AssertorResult<>(supplier.get());
    }

    protected static <X, T> Supplier<AssertorResult<T>> and(final Supplier<AssertorResult<X>> step, final T object) {
        return () -> new AssertorResult<>(step.get(), object, EnumOperator.AND);
    }

    protected static <X, T> Supplier<AssertorResult<T>> or(final Supplier<AssertorResult<X>> step, final T object) {
        return () -> new AssertorResult<>(step.get(), object, EnumOperator.OR);
    }

    protected static <X, T> Supplier<AssertorResult<T>> xor(final Supplier<AssertorResult<X>> step, final T object) {
        return () -> new AssertorResult<>(step.get(), object, EnumOperator.XOR);
    }

    protected static <T> Supplier<AssertorResult<T>> and(final Supplier<AssertorResult<T>> step) {
        return () -> new AssertorResult<>(step.get(), EnumOperator.AND);
    }

    protected static <T> Supplier<AssertorResult<T>> or(final Supplier<AssertorResult<T>> step) {
        return () -> new AssertorResult<>(step.get(), EnumOperator.OR);
    }

    protected static <T> Supplier<AssertorResult<T>> xor(final Supplier<AssertorResult<T>> step) {
        return () -> new AssertorResult<>(step.get(), EnumOperator.XOR);
    }

    protected static <T, X> Supplier<AssertorResult<T>> and(final Supplier<AssertorResult<T>> step,
            final Supplier<AssertorResult<X>> other) {
        return () -> new AssertorResult<>(step.get(), other.get(), EnumOperator.AND);
    }

    protected static <T, X> Supplier<AssertorResult<T>> or(final Supplier<AssertorResult<T>> step,
            final Supplier<AssertorResult<X>> other) {
        return () -> new AssertorResult<>(step.get(), other.get(), EnumOperator.OR);
    }

    protected static <T, X> Supplier<AssertorResult<T>> xor(final Supplier<AssertorResult<T>> step,
            final Supplier<AssertorResult<X>> other) {
        return () -> new AssertorResult<>(step.get(), other.get(), EnumOperator.XOR);
    }

    @SafeVarargs
    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> prepareStep(final Supplier<AssertorResult<T>> supplier,
            final Function<T, Boolean> precondition, final BiFunctionThrowable<T, Boolean, Boolean, E> checker,
            final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage,
            final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> message, final boolean notAppliedByChecker,
            final Pair<Object, EnumType>... parameters) {

        return () -> {
            final AssertorResult<T> result = supplier.get();

            int objectIndex = 0;
            final int paramSize = result.getParameters().size();
            for (int i = paramSize - 1; i >= 0; i--) {
                if (result.getParameters().get(i).getRight()) {
                    objectIndex = i;
                    break;
                }
            }

            result.getParameters().addAll(CollectionUtils2.transformIntoList(parameters, PAIR_TO_TRIPLE_TRANSFORMER));

            final boolean previousPrecondition = result.isPreconditionOK();
            final boolean currentPrecondition = precondition == null || precondition.apply(result.getObject());

            final boolean newPrecondition;
            final boolean newValid;
            final CharSequence newPreconditionMessage;
            final CharSequence newMessage;

            if (!previousPrecondition || !currentPrecondition) {
                final StringBuilder sb = new StringBuilder();
                if (!previousPrecondition && !currentPrecondition) {
                    sb.append(result.getPreconditionMessage()).append(EnumOperator.AND)
                            .append(preconditionMessage.apply(result, objectIndex, paramSize));
                } else if (!previousPrecondition) {
                    sb.append(result.getPreconditionMessage());
                } else {
                    sb.append(preconditionMessage.apply(result, objectIndex, paramSize));
                }

                newPrecondition = false;
                newPreconditionMessage = sb;
                newValid = false;
                newMessage = EMPTY_STRING;
            } else {
                final boolean previousOK = result.isValid();
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
                    currentOK = result.isNot();
                }

                newPrecondition = true;
                newPreconditionMessage = EMPTY_STRING;
                newValid = isValid(previousOK, currentOK, result.getOperator());
                newMessage = getErrors(newValid, previousOK, currentOK, result, message, objectIndex, paramSize);
            }

            return new AssertorResult<>(result, newPrecondition, newValid, newPreconditionMessage, newMessage);
        };
    }

    protected static boolean isValid(final boolean previousOK, final boolean currentOK, final EnumOperator operator) {
        boolean OK = false;
        if (EnumOperator.AND.equals(operator) || operator == null) {
            OK = previousOK & currentOK;
        } else if (EnumOperator.OR.equals(operator)) {
            OK = previousOK | currentOK;
        } else if (EnumOperator.XOR.equals(operator)) {
            OK = previousOK ^ currentOK;
        }
        return OK;
    }

    private static <T> CharSequence getErrors(final boolean OK, final boolean previousOK, final boolean currentOK,
            final AssertorResult<T> result, final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> message,
            final int objectIndex, final int paramSize) {
        final StringBuilder sb = new StringBuilder();
        if (!OK) {
            if (result.getOperator() != null && !previousOK) {
                sb.append(result.getMessage());
                if (!currentOK) {
                    sb.append(result.getOperator());
                }
            }
            if (!currentOK && message != null) {
                sb.append(message.apply(result, objectIndex, paramSize, result.isNot()));
            }
        }
        return sb;
    }
}
