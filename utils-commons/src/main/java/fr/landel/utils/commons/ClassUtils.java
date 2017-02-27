/*
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

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Queue;
import java.util.Set;
import java.util.function.Function;

/**
 * Utility class to manage classes.
 *
 * @since Nov 27, 2015
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

    /**
     * Get the class of the object (null safe).
     * 
     * @param object
     *            The object (required)
     * @param <T>
     *            The object type
     * @return The class of the object or {@code null}
     */
    public static <T> Class<T> getClass(final T object) {
        return CastUtils.getClass(object);
    }

    /**
     * Check if the class of the object is assignable from the specified class
     * (null safe).
     * 
     * @param clazz
     *            The class or super class
     * @param object
     *            The object (required)
     * @return true, if assignable from
     */
    public static boolean isAssignableFrom(final Class<?> clazz, final Object object) {
        if (clazz != null) {
            Class<?> objClass = CastUtils.getClass(object);
            if (objClass != null) {
                return clazz.isAssignableFrom(objClass);
            }
        }
        return false;
    }

    /**
     * Get the class name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @return The string
     */
    public static String getName(final Object object) {
        return ClassUtils.getName(object, (c) -> c.getName());
    }

    /**
     * Get the simple class name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @return The string
     */
    public static String getSimpleName(final Object object) {
        return ClassUtils.getName(object, (c) -> c.getSimpleName());
    }

    /**
     * Get the canonical class name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @return The string
     */
    public static String getCanonicalName(final Object object) {
        return ClassUtils.getName(object, (c) -> c.getCanonicalName());
    }

    /**
     * Get the type name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @return The string
     */
    public static String getTypeName(final Object object) {
        return ClassUtils.getName(object, (c) -> c.getTypeName());
    }

    /**
     * Get the package class name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @return The string
     */
    public static String getPackageName(final Object object) {
        return ClassUtils.getName(object, (c) -> c.getPackage().getName());
    }

    /**
     * Get the class name or the string "{@code null}"
     * 
     * @param object
     *            The object to check
     * @param function
     *            The function to extract the name from class
     * @return The string
     */
    private static String getName(final Object object, final Function<Class<?>, String> function) {
        final String clazzName;
        if (object != null) {
            clazzName = function.apply(object.getClass());
        } else {
            clazzName = "null";
        }
        return clazzName;
    }
}
