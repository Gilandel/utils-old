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

import java.util.function.BiPredicate;
import java.util.function.Predicate;

import fr.landel.utils.commons.ArrayUtils;

/**
 * Utility class to prepare the check of arrays
 *
 * @since Aug 10, 2016
 * @author Gilles
 *
 */
public class AssertorArray extends Constants {

    /**
     * Prepare the next step to validate the array length
     * 
     * @param step
     *            the current step
     * @param length
     *            the length to validate
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> hasLength(final StepAssertor<T[]> step, final int length, final Message message) {

        final Predicate<T[]> preChecker = (object) -> length >= 0 && object != null;

        final BiPredicate<T[], Boolean> checker = (object, not) -> object.length == length;

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.ARRAY.LENGTH, false,
                new Parameter<>(length, EnumType.NUMBER_INTEGER));
    }

    /**
     * Prepare the next step to validate if the array is {@code null} or empty
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> isEmpty(final StepAssertor<T[]> step, final Message message) {

        final BiPredicate<T[], Boolean> checker = (object, not) -> ArrayUtils.isEmpty(object);

        return new StepAssertor<>(step, checker, false, message, MSG.ARRAY.EMPTY, false);
    }

    /**
     * Prepare the next step to validate if the array is NOT {@code null} and
     * NOT empty
     * 
     * @param step
     *            the current step
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> isNotEmpty(final StepAssertor<T[]> step, final Message message) {

        final BiPredicate<T[], Boolean> checker = (object, not) -> ArrayUtils.isNotEmpty(object);

        return new StepAssertor<>(step, checker, false, message, MSG.ARRAY.EMPTY, true);
    }

    /**
     * Prepare the next step to validate if the array contains the element
     * 
     * @param step
     *            the current step
     * @param element
     *            the element to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> contains(final StepAssertor<T[]> step, final T element, final Message message) {

        final Predicate<T[]> preChecker = (object) -> object != null;

        final BiPredicate<T[], Boolean> checker = (object, not) -> AssertorArray.has(object, element);

        return new StepAssertor<>(step, preChecker, checker, false, message, MSG.ARRAY.CONTAINS_OBJECT, false,
                new Parameter<>(element, EnumType.UNKNOWN));
    }

    private static <T> StepAssertor<T[]> contains(final StepAssertor<T[]> step, final T[] array, final boolean all, final CharSequence key,
            final Message message) {

        final Predicate<T[]> preChecker = (object) -> array != null && object != null;

        final BiPredicate<T[], Boolean> checker = (object, not) -> AssertorArray.has(object, array, all, not);

        return new StepAssertor<>(step, preChecker, checker, true, message, key, false, new Parameter<>(array, EnumType.ARRAY));
    }

    /**
     * Prepare the next step to validate if the array contains all elements of
     * the specified array
     * 
     * @param step
     *            the current step
     * @param array
     *            the array to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> containsAll(final StepAssertor<T[]> step, final T[] array, final Message message) {

        return AssertorArray.contains(step, array, true, MSG.ARRAY.CONTAINS_ALL, message);
    }

    /**
     * Prepare the next step to validate if the array contains any elements of
     * the specified array
     * 
     * @param step
     *            the current step
     * @param array
     *            the array to find
     * @param message
     *            the message if invalid
     * @param <T>
     *            the array elements type
     * @return the next step
     */
    protected static <T> StepAssertor<T[]> containsAny(final StepAssertor<T[]> step, final T[] array, final Message message) {

        return AssertorArray.contains(step, array, false, MSG.ARRAY.CONTAINS_ANY, message);
    }

    private static <T> boolean has(final T[] array, final T object) {
        if (object != null) {
            for (T objectArray : array) {
                if (object.equals(objectArray)) {
                    return true;
                }
            }
        } else {
            for (T objectArray : array) {
                if (objectArray == null) {
                    return true;
                }
            }
        }
        return false;
    }

    private static <T> boolean has(final T[] array1, final T[] array2, final boolean all, final boolean not) {
        int found = 0;
        for (T objectArray : array2) {
            if (AssertorArray.has(array1, objectArray)) {
                ++found;
            }
        }

        return HelperAssertor.isValid(all, not, found, array2.length);
    }
}