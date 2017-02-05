/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

import java.util.Objects;

/**
 * Operations on arrays, primitive arrays (like {@code int[]}) and primitive
 * wrapper arrays (like {@code Integer[]}).
 *
 * <p>
 * This class tries to handle {@code null} input gracefully. An exception will
 * not be thrown for a {@code null} array input. However, an Object array that
 * contains a {@code null} element may throw an exception. Each method documents
 * its behaviour.
 * </p>
 *
 * <p>
 * #ThreadSafe#
 * </p>
 *
 * @since Aug 2, 2016
 * @author Gilles
 *
 */
public final class ArrayUtils extends org.apache.commons.lang3.ArrayUtils {

    /**
     * Hidden constructor
     */
    private ArrayUtils() {
        super();
    }

    /**
     * Search if {@code arrayToSearch} contains all {@code arraySearched}
     * entries.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if all elements were found
     */
    public static <T, U> boolean containsAll(final T[] arrayToSearch, final U[] arraySearched) {
        return containsAll(arrayToSearch, arraySearched, true);
    }

    /**
     * Search if {@code arrayToSearch} contains all {@code arraySearched}
     * entries.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param checkType
     *            check if the type is identical from each array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if all elements were found
     */
    public static <T, U> boolean containsAll(final T[] arrayToSearch, final U[] arraySearched, final boolean checkType) {
        Objects.requireNonNull(arrayToSearch, "Array to search cannot be null");
        Objects.requireNonNull(arraySearched, "Searched array cannot be null");

        return has(arrayToSearch, arraySearched, true, checkType);
    }

    /**
     * Search if {@code arrayToSearch} contains any {@code arraySearched}
     * entries.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if at least one element was found
     */
    public static <T, U> boolean containsAny(final T[] arrayToSearch, final U[] arraySearched) {
        return containsAny(arrayToSearch, arraySearched, true);
    }

    /**
     * Search if {@code arrayToSearch} contains any {@code arraySearched}
     * entries.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param checkType
     *            check if the type is identical from each array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if at least one element was found
     */
    public static <T, U> boolean containsAny(final T[] arrayToSearch, final U[] arraySearched, final boolean checkType) {
        Objects.requireNonNull(arrayToSearch, "Array to search cannot be null");
        Objects.requireNonNull(arraySearched, "Searched array cannot be null");

        return has(arrayToSearch, arraySearched, false, checkType);
    }

    /**
     * Count the number of {@code arraySearched} in {@code arrayToSearch}.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return number, the count of elements found
     */
    public static <T, U> int count(final T[] arrayToSearch, final U[] arraySearched) {
        return count(arrayToSearch, arraySearched, true);
    }

    /**
     * Count the number of {@code arraySearched} in {@code arrayToSearch}.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param arraySearched
     *            what to search array (required, not null)
     * @param checkType
     *            check if the type is identical from each array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return number, the count of elements found
     */
    public static <T, U> int count(final T[] arrayToSearch, final U[] arraySearched, final boolean checkType) {
        Objects.requireNonNull(arrayToSearch, "Array to search cannot be null");
        Objects.requireNonNull(arraySearched, "Searched array cannot be null");

        return count(arrayToSearch, arraySearched, checkType, false);
    }

    /**
     * Count the number of object in {@code arrayToSearch}.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param object
     *            what to search
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element to search
     * @return the number of iterations
     */
    public static <T, U> int count(final T[] arrayToSearch, final U object) {
        return count(arrayToSearch, object, true);
    }

    /**
     * Count the number of object in {@code arrayToSearch}.
     * 
     * @param arrayToSearch
     *            where to search array (required, not null)
     * @param object
     *            what to search
     * @param checkType
     *            check if the type is identical from each array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element to search
     * @return the number of iterations
     */
    public static <T, U> int count(final T[] arrayToSearch, final U object, final boolean checkType) {
        Objects.requireNonNull(arrayToSearch, "Array to search cannot be null");

        int found = 0;
        if (object == null) {
            for (T objectArray : arrayToSearch) {
                if (objectArray == null) {
                    ++found;
                }
            }
        } else if (!checkType || arrayToSearch.getClass().getComponentType().isInstance(object)) {
            for (T objectArray : arrayToSearch) {
                if (object.equals(objectArray)) {
                    ++found;
                }
            }
        }
        return found;
    }

    private static <T, U> int count(final T[] arrayToSearch, final U[] arraySearched, final boolean checkType, final boolean stopOnFirst) {
        if (checkType && !arrayToSearch.getClass().getComponentType().isAssignableFrom(arraySearched.getClass().getComponentType())) {
            return 0;
        }

        if (stopOnFirst) {
            for (U objectArray : arraySearched) {
                if (has(arrayToSearch, objectArray)) {
                    return 1;
                }
            }
            return 0;
        } else {
            int found = 0;
            for (U objectArray : arraySearched) {
                if (has(arrayToSearch, objectArray)) {
                    ++found;
                }
            }
            return found;
        }
    }

    private static <T, U> boolean has(final T[] array, final U object) {
        if (object == null) {
            for (T objectArray : array) {
                if (objectArray == null) {
                    return true;
                }
            }
        } else {
            for (T objectArray : array) {
                if (object.equals(objectArray)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static <T, U> boolean has(final T[] arrayToSearch, final U[] arraySearched, final boolean all, final boolean checkType) {
        if (checkType && !arrayToSearch.getClass().getComponentType().isAssignableFrom(arraySearched.getClass().getComponentType())) {
            return false;
        }

        if (all) {
            return count(arrayToSearch, arraySearched, false, false) == arraySearched.length;
        } else {
            return count(arrayToSearch, arraySearched, false, true) > 0;
        }
    }
}
