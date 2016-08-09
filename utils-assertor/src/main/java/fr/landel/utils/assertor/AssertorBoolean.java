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

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.TriFunction;

public class AssertorBoolean extends Constants {

    protected static <E extends Throwable> AssertorResult<Boolean> isTrue(final AssertorResult<Boolean> result, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Boolean, Boolean, Boolean, E> checker = (bool, not) -> Boolean.TRUE.equals(bool);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.BOOLEAN.TRUE, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }

    protected static <E extends Throwable> AssertorResult<Boolean> isFalse(final AssertorResult<Boolean> result, final Locale locale,
            final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Boolean, Boolean, Boolean, E> checker = (bool, not) -> Boolean.FALSE.equals(bool);

        final TriFunction<Integer, Integer, Boolean, CharSequence> builtMessage = (objectIndex, paramIndex, not) -> HelperMessage
                .getMessage(result, locale, message, arguments, MSG.BOOLEAN.FALSE, not, objectIndex);

        return HelperAssertor.combine(result, null, checker, null, builtMessage, false);
    }
}
