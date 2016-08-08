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

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorClass extends AssertorConstants {

    protected static <T, E extends Throwable> Supplier<AssertorResult<Class<T>>> isAssignableFrom(
            final Supplier<AssertorResult<Class<T>>> step, final Class<?> superType, final Locale locale, final CharSequence message,
            final Object[] arguments) {

        final Function<Class<T>, Boolean> precondition = (type) -> type != null && superType != null;

        final TriFunction<AssertorResult<Class<T>>, Integer, Integer, CharSequence> preconditionMessage = (result, objectIndex,
                paramIndex) -> AssertorHelper.msg(result, MSG.CLASS.ASSIGNABLE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Class<T>, Boolean, Boolean, E> checker = (type, not) -> superType.isAssignableFrom(type);

        final QuadFunction<AssertorResult<Class<T>>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex,
                not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.CLASS.ASSIGNABLE, not, objectIndex, paramIndex);

        return AssertorHelper.prepareStep(step, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(superType, EnumType.CLASS));
    }
}
