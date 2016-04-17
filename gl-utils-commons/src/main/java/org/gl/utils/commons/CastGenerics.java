/*
 * #%L
 * gl-utils-commons
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package org.gl.utils.commons;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utility class to cast classes.
 *
 * @since 23 nov. 2015
 * @author Gilles Landel
 *
 */
public final class CastGenerics {

    private static final Logger LOGGER = LoggerFactory.getLogger(CastGenerics.class);

    /**
     * Hidden constructor.
     */
    private CastGenerics() {
    }

    /**
     * Get the class of the object.
     * 
     * @param object
     *            The object
     * @param <T>
     *            The object type
     * @return The class of the object
     */
    @SuppressWarnings("unchecked")
    public static <T> Class<T> getObjectClass(final T object) {
        if (object != null) {
            return (Class<T>) object.getClass();
        }
        return null;
    }

    /**
     * Cast an object into the specified class.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> T getObject(final Object o, Class<T> clazz) {
        if (o != null && clazz.isAssignableFrom(o.getClass())) {
            return clazz.cast(o);
        }
        return null;
    }

    /**
     * Cast an object into the specified class.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> List<T> getList(final Object o, final Class<T> clazz) {
        final List<T> list = new ArrayList<>();

        if (o != null && List.class.isAssignableFrom(o.getClass())) {
            List<?> mObj = (List<?>) o;
            for (Object obj : mObj) {
                setListValue(list, obj, clazz);
            }
        }

        return list;
    }

    /**
     * Cast an object into a typed map.
     * 
     * @param o
     *            The input object
     * @param classKey
     *            The class of the key
     * @param classValue
     *            The class of the value
     * @return The casted object
     * @param <K>
     *            The type of key
     * @param <V>
     *            The type of the value
     */
    public static <K, V> Map<K, V> getHashMap(final Object o, final Class<K> classKey, final Class<V> classValue) {
        final Map<K, V> map = new HashMap<>();

        if (o != null && Map.class.isAssignableFrom(o.getClass())) {
            Map<?, ?> mObj = (Map<?, ?>) o;
            for (Entry<?, ?> obj : mObj.entrySet()) {
                if (obj.getKey() != null && classKey.isAssignableFrom(obj.getKey().getClass())) {
                    final K key = classKey.cast(obj.getKey());
                    setMapValue(map, key, obj, classValue);
                }
            }
        }

        return map;
    }

    /**
     * Set the map value.
     * 
     * @param map
     *            the map
     * @param key
     *            the key
     * @param obj
     *            the object
     * @param classValue
     *            the class value
     * @param <K>
     *            The type of key
     * @param <V>
     *            The type of the value
     * 
     */
    private static <K, V> void setMapValue(final Map<K, V> map, final K key, final Entry<?, ?> obj, final Class<V> classValue) {
        if (obj.getValue() != null && classValue.isAssignableFrom(obj.getValue().getClass())) {
            map.put(key, classValue.cast(obj.getValue()));
        } else if (obj.getValue() == null) {
            map.put(key, null);
        }
    }

    /**
     * Set the list value.
     * 
     * @param list
     *            the list
     * @param obj
     *            the object
     * @param clazz
     *            the class
     * @param <T>
     *            The type of the element
     */
    private static <T> void setListValue(final List<T> list, final Object obj, final Class<T> clazz) {
        if (obj != null && clazz.isAssignableFrom(obj.getClass())) {
            list.add(clazz.cast(obj));
        } else if (obj == null) {
            list.add(null);
        }
    }

    /**
     * Cast an object into a typed set.
     * 
     * @param o
     *            The input object
     * @param classElement
     *            The class of the element in the set
     * @return The casted object
     * @param <T>
     *            The type of the element
     */
    public static <T> Set<T> getHashSet(final Object o, final Class<T> classElement) {
        final Set<T> set = new HashSet<>();

        if (o != null && Set.class.isAssignableFrom(o.getClass())) {
            Set<?> mObj = (Set<?>) o;
            for (Object obj : mObj) {
                if (classElement.isAssignableFrom(obj.getClass())) {
                    final T element = classElement.cast(obj);
                    set.add(element);
                }
            }
        }

        return set;
    }

    /**
     * Cast an object into a typed iterator.
     * 
     * @param o
     *            The input object
     * @param classElement
     *            The class of the element in the iterator
     * @return The casted object
     * @param <T>
     *            The type of the element
     */
    public static <T> Iterator<T> getIterator(final Object o, final Class<T> classElement) {
        final List<T> list = new ArrayList<>();

        if (o != null && Iterator.class.isAssignableFrom(o.getClass())) {
            Iterator<?> mObj = (Iterator<?>) o;
            while (mObj.hasNext()) {
                Object obj = mObj.next();
                if (obj != null && classElement.isAssignableFrom(obj.getClass())) {
                    final T element = classElement.cast(obj);
                    list.add(element);
                } else if (obj == null) {
                    list.add(null);
                }
            }
        }

        return list.iterator();
    }

    /**
     * Instanciate some objects.
     * 
     * @param logLevelInfo
     *            log level info
     * @param constructor
     *            constructor
     * @param instanciableClass
     *            instantiable class
     * @param objects
     *            list of objects
     * @return the instanciated class
     * @param <T>
     *            The type of the element
     */
    @SuppressWarnings("unchecked")
    private static <T> T instanciate(final boolean logLevelInfo, final Constructor<?> constructor, final Class<T> instanciableClass,
            final Object... objects) {
        T result = null;
        Object resultTmp;
        try {
            if (constructor != null) {
                resultTmp = constructor.newInstance(objects);
                if (resultTmp != null && instanciableClass.isAssignableFrom(resultTmp.getClass())) {
                    result = (T) resultTmp;
                }
            }
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            if (logLevelInfo) {
                LOGGER.info("Cannot instanciate " + constructor.getName());
            } else {
                LOGGER.error("Cannot instanciate " + constructor.getName());
            }
        }
        return result;
    }

    /**
     * Instanciate a typed constructor.
     * 
     * @param instanciableClass
     *            instanciable class
     * @param classes
     *            classes
     * @param objects
     *            objects
     * @return the instanciated constructor
     * @param <T>
     *            The type of the element
     */
    private static <T> T instanciateTypedConstructor(final Class<T> instanciableClass, final List<Class<?>> classes, final Object[] objects) {
        T result = null;

        Constructor<T> typedConstructor;
        try {
            typedConstructor = instanciableClass.getDeclaredConstructor(classes.toArray(new Class<?>[classes.size()]));

            result = instanciate(true, typedConstructor, instanciableClass, objects);
        } catch (NoSuchMethodException | SecurityException e) {
            LOGGER.info("Cannot map [" + StringUtils.join(objects, ", ") + "] into " + instanciableClass.getName(), e);
        }
        return result;
    }

    /**
     * Instanciate an unknown constructor.
     * 
     * @param instanciableClass
     *            instanciable class
     * @param classes
     *            classes
     * @param objects
     *            objects
     * @return the instanciated constructor
     * @param <T>
     *            The type of the element
     */
    private static <T> T instanciateUnknownConstructor(final Class<T> instanciableClass, final List<Class<?>> classes,
            final Object[] objects) {
        T result = null;

        try {
            Constructor<?>[] constructors = instanciableClass.getDeclaredConstructors();
            boolean mismatch = true;

            for (Constructor<?> constructor : constructors) {
                Class<?>[] parameterTypes = constructor.getParameterTypes();
                if (parameterTypes.length == classes.size()) {
                    mismatch = false;
                    for (int i = 0; i < parameterTypes.length; i++) {
                        mismatch |= classes.get(i) != null && !parameterTypes[i].isAssignableFrom(classes.get(i));
                    }
                    if (!mismatch) {
                        result = instanciate(false, constructor, instanciableClass, objects);
                        break;
                    }
                }
            }
        } catch (SecurityException e) {
            LOGGER.error("Cannot map [" + StringUtils.join(objects, ", ") + "] into " + instanciableClass.getName(), e);
        }

        return result;
    }

    /**
     * Map a list of object into a instanciable.
     * 
     * @param instanciableClass
     *            The instanciable
     * @param objects
     *            The objects to pass through the constructor
     * @return The new instance
     * @param <T>
     *            The result class
     */
    public static <T> T map(final Class<T> instanciableClass, final Object... objects) {
        List<Class<?>> classes = new ArrayList<>();
        T result = null;

        if (instanciableClass != null) {
            for (Object object : objects) {
                if (object != null) {
                    classes.add(object.getClass());
                } else {
                    classes.add(null);
                }
            }

            result = instanciateTypedConstructor(instanciableClass, classes, objects);

            if (result == null) {
                result = instanciateUnknownConstructor(instanciableClass, classes, objects);
            }
        }

        return result;
    }

    /**
     * Get the generic collection class.
     * 
     * @param instanciableClass
     *            The item class
     * @param <T>
     *            The item type
     * @return The iterable typed
     */
    @SuppressWarnings("unchecked")
    public static <T> Class<List<T>> getListTypedClass(final Class<T> instanciableClass) {
        final List<T> list = new ArrayList<>();

        try {
            list.add(instanciableClass.newInstance());
        } catch (InstantiationException | IllegalAccessException e) {
            LOGGER.error("Errors occurred in CastGenerics#getListTypedClass()", e);
        }

        return (Class<List<T>>) list.getClass();
    }
}
