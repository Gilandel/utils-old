/*-
 * #%L
 * utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.commons;

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
 * @since 2 août 2016
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
     *            where to search array
     * @param arraySearched
     *            what to search array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if all elements were found
     */
    public static <T, U> boolean containsAll(final T[] arrayToSearch, final U[] arraySearched) {
        return has(arrayToSearch, arraySearched, true);
    }

    /**
     * Search if {@code arrayToSearch} contains any {@code arraySearched}
     * entries.
     * 
     * @param arrayToSearch
     *            where to search array
     * @param arraySearched
     *            what to search array
     * @param <T>
     *            The type of element in array to search
     * @param <U>
     *            The type of element in searched array
     * @return true, if at least one element was found
     */
    public static <T, U> boolean containsAny(final T[] arrayToSearch, final U[] arraySearched) {
        return has(arrayToSearch, arraySearched, false);
    }

    private static <T, U> boolean has(final T[] array, final U object) {
        boolean found = false;
        if (object != null) {
            for (T objectArray : array) {
                if (object.equals(objectArray)) {
                    found = true;
                    break;
                }
            }
        } else {
            for (T objectArray : array) {
                if (objectArray == null) {
                    found = true;
                    break;
                }
            }
        }
        return found;
    }

    private static <T, U> boolean has(final T[] array1, final U[] array2, final boolean all) {
        int found = 0;
        for (U objectArray : array2) {
            if (has(array1, objectArray)) {
                found++;
            }
        }
        if (all) {
            return found == array1.length;
        } else {
            return found > 0;
        }
    }
}