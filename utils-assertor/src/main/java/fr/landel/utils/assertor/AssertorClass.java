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

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorClass extends Constants {

    protected static <T, E extends Throwable> AssertorResult<Class<T>> isAssignableFrom(final AssertorResult<Class<T>> result,
            final Class<?> superType, final Locale locale, final CharSequence message, final Object[] arguments) {

        final Function<Class<T>, Boolean> precondition = (type) -> type != null && superType != null;

        final BiFunction<Integer, Integer, CharSequence> preconditionMessage = (objectIndex, paramIndex) -> HelperMessage.getDefaultMessage(result,
                MSG.CLASS.ASSIGNABLE, true, false, objectIndex, paramIndex);

        final BiFunctionThrowable<Class<T>, Boolean, Boolean, E> checker = (type, not) -> superType.isAssignableFrom(type);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.CLASS.ASSIGNABLE, not, objectIndex, paramIndex);

        return HelperAssertor.combine(result, precondition, checker, preconditionMessage, builtMessage, false,
                Pair.of(superType, EnumType.CLASS));
    }
}
