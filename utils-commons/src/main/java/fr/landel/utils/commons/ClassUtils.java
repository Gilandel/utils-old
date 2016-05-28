/*
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

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Queue;
import java.util.Set;

/**
 * Utility class to manage classes.
 *
 * @since 27 nov. 2015
 * @author Gilles Landel
 *
 */
public final class ClassUtils {

    /**
     * Hidden constructor.
     */
    private ClassUtils() {
    }

    /**
     * List super classes
     * 
     * @param clazz
     *            The class to check
     * @return The list of super classes
     */
    public static Set<Class<?>> getSuperclasses(final Class<?> clazz) {
        final Set<Class<?>> result = new LinkedHashSet<>();
        final Queue<Class<?>> queue = new ArrayDeque<>();
        queue.add(clazz);
        if (clazz.isInterface()) {
            queue.add(Object.class);
        }
        while (!queue.isEmpty()) {
            Class<?> c = queue.remove();
            if (result.add(c)) {
                Class<?> sup = c.getSuperclass();
                if (sup != null) {
                    queue.add(sup);
                }
                queue.addAll(Arrays.asList(c.getInterfaces()));
            }
        }
        return result;
    }

    /**
     * Return the list of common classes
     * 
     * @param class1
     *            The first class
     * @param class2
     *            The second class
     * @return The list of common classes
     */
    public static Set<Class<?>> getCommonSuperclasses(final Class<?> class1, final Class<?> class2) {
        final Set<Class<?>> superClasses1 = ClassUtils.getSuperclasses(class1);
        final Set<Class<?>> superClasses2 = ClassUtils.getSuperclasses(class2);
        final Set<Class<?>> result = new LinkedHashSet<>();
        for (Class<?> clazz : superClasses1) {
            if (superClasses2.contains(clazz)) {
                result.add(clazz);
            }
        }
        return result;
    }

    /**
     * Check if one of the objects is null
     * 
     * @param objects
     *            The objects list
     * @return true, if one is null
     */
    public static boolean isAnyNull(final Object... objects) {
        if (objects != null) {
            for (Object object : objects) {
                if (object == null) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Check if all of the objects are null
     * 
     * @param objects
     *            The objects list
     * @return true, if all are null
     */
    public static boolean isAllNull(Object... objects) {
        if (objects != null && objects.length > 0) {
            for (Object object : objects) {
                if (object != null) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * Check if all of the objects are NOT null
     * 
     * @param objects
     *            The objects list
     * @return true, if all are NOT null
     */
    public static boolean isNoneNull(final Object... objects) {
        if (objects != null && objects.length > 0) {
            return !ClassUtils.isAnyNull(objects);
        }
        return false;
    }
}
