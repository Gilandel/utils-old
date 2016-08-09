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
import java.util.function.BiFunction;
import java.util.function.Function;

import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.commons.ClassUtils;
import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.PredicateThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorObject extends Constants {

    protected static <T, E extends Throwable> AssertorResult<T> isNull(final AssertorResult<T> result, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object == null;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.NULL, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> AssertorResult<T> isNotNull(final AssertorResult<T> result, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> object != null;

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.NULL, !not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <T, E extends Throwable> AssertorResult<T> isEqual(final AssertorResult<T> result, final Object object,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object1, not) -> isEqualInternal(object1, object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.EQUALS, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(object, EnumType.getType(object)));
    }

    protected static <T, E extends Throwable> AssertorResult<T> isNotEqual(final AssertorResult<T> result, final Object object,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object1, not) -> !isEqualInternal(object1, object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.EQUALS, !not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false, Pair.of(object, EnumType.getType(object)));
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

    protected static <T, E extends Throwable> AssertorResult<T> isInstanceOf(final AssertorResult<T> result, final Class<?> type,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && type != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.OBJECT.INSTANCE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> type.isInstance(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.INSTANCE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(type, EnumType.CLASS));
    }

    protected static <T, E extends Throwable> AssertorResult<T> isAssignableFrom(final AssertorResult<T> result, final Class<?> type,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> object != null && type != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.OBJECT.ASSIGNABLE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> type.isAssignableFrom(ClassUtils.getClass(object));

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.ASSIGNABLE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(type, EnumType.CLASS));
    }

    protected static <T, E extends Throwable> AssertorResult<T> validates(final AssertorResult<T> result,
            final PredicateThrowable<T, E> predicate, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<T, Boolean> precondition = (object) -> predicate != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.OBJECT.VALIDATES, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<T, Boolean, Boolean, E> checker = (object, not) -> predicate.testThrows(object);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.OBJECT.VALIDATES, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(predicate, EnumType.UNKNOWN));
    }
}