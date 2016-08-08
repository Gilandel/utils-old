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

import java.util.Locale;
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.ClassUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.PredicateThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorObject extends AssertorConstants {

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isNull(final Supplier<AssertorResult<T>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object == null;

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.NULL, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isNotNull(final Supplier<AssertorResult<T>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object != null;

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.NULL, !not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isEqual(final Supplier<AssertorResult<T>> step,
            final Object object, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object1, not) -> isEqualInternal(object1, object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.EQUALS, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(object, EnumType.getType(object)));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isNotEqual(final Supplier<AssertorResult<T>> step,
            final Object object, final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object1, not) -> !isEqualInternal(object1, object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.EQUALS, !not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false, Pair.of(object, EnumType.getType(object)));
    }

    private static <T, E extends Throwable> boolean isEqualInternal(final T object1, final Object object2) {
        boolean result = false;
        if (object1 == object2) {
            result = true;
        } else if (object1 != null && object1.equals(object2)) {
            result = true;
        } else if (ClassUtils.isAssignableFrom(CharSequence.class, object1) && ClassUtils.isAssignableFrom(CharSequence.class, object2)
                && object1.toString().equals(object2.toString())) {
            result = true;
        }
        return result;
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isInstanceOf(final Supplier<AssertorResult<T>> step,
            final Class<?> type, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && type != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.OBJECT.INSTANCE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> type.isInstance(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.INSTANCE, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(type, EnumType.CLASS));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> isAssignableFrom(final Supplier<AssertorResult<T>> step,
            final Class<?> type, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && type != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.OBJECT.ASSIGNABLE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> type.isAssignableFrom(ClassUtils.getClass(object));

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.ASSIGNABLE, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(type, EnumType.CLASS));
    }

    protected static <T, E extends Throwable> Supplier<AssertorResult<T>> validates(final Supplier<AssertorResult<T>> step,
            final PredicateThrowable<T, E> predicate, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> predicate != null;

        final TriFunction<AssertorResult<T>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.OBJECT.VALIDATES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> predicate.testThrows(object);

        final QuadFunction<AssertorResult<T>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex, paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.OBJECT.VALIDATES, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(predicate, EnumType.UNKNOWN));
    }
}