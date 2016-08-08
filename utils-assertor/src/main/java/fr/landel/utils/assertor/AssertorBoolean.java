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
import java.util.function.Supplier;

import fr.landel.utils.commons.function.BiFunctionThrowable;
import fr.landel.utils.commons.function.QuadFunction;

public class AssertorBoolean extends AssertorConstants {

    protected static <E extends Throwable> Supplier<AssertorResult<Boolean>> isTrue(final Supplier<AssertorResult<Boolean>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Boolean, Boolean, Boolean, E> checker = (bool, not) -> Boolean.TRUE.equals(bool);

        final QuadFunction<AssertorResult<Boolean>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.BOOLEAN.TRUE, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }

    protected static <E extends Throwable> Supplier<AssertorResult<Boolean>> isFalse(final Supplier<AssertorResult<Boolean>> step,
            final Locale locale, final CharSequence message, final Object[] arguments) {

        final BiFunctionThrowable<Boolean, Boolean, Boolean, E> checker = (bool, not) -> Boolean.FALSE.equals(bool);

        final QuadFunction<AssertorResult<Boolean>, Integer, Integer, Boolean, CharSequence> builtMessage = (result, objectIndex,
                paramIndex, not) -> AssertorHelper.getMessage(result, locale, message, arguments, MSG.BOOLEAN.FALSE, not, objectIndex);

        return AssertorHelper.prepareStep(step, null, checker, null, builtMessage, false);
    }
}
