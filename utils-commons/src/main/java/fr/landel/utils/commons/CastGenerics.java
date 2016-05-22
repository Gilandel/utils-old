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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

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
    public static <T> Class<T> getClass(final T object) {
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
    public static <T> T cast(final Object o, Class<T> clazz) {
        if (o != null && clazz.isAssignableFrom(o.getClass())) {
            return clazz.cast(o);
        }
        return null;
    }

    /**
     * Cast an object into a typed array list.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> List<T> getArrayList(final Object o, final Class<T> clazz) {
        final List<T> list = new ArrayList<>();

        list(list, o, clazz);

        return list;
    }

    /**
     * Cast an object into a typed vector.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> List<T> getVector(final Object o, final Class<T> clazz) {
        final List<T> list = new Vector<>();

        list(list, o, clazz);

        return list;
    }

    /**
     * Cast an object into a typed linked list as list.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> List<T> getLinkedListAsList(final Object o, final Class<T> clazz) {
        final List<T> list = new LinkedList<>();

        list(list, o, clazz);

        return list;
    }

    /**
     * Cast an object into a typed linked list as queue.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> Queue<T> getLinkedListAsQueue(final Object o, final Class<T> clazz) {
        final Queue<T> queue = new LinkedList<>();

        queue(queue, o, clazz, false);

        return queue;
    }

    /**
     * Cast an object into a typed priority queue.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> Queue<T> getPriorityQueue(final Object o, final Class<T> clazz) {
        final Queue<T> queue = new PriorityQueue<>();

        queue(queue, o, clazz, true);

        return queue;
    }

    /**
     * Cast an object into a typed linked blocking queue.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> BlockingQueue<T> getLinkedBlockingQueue(final Object o, final Class<T> clazz) {
        final BlockingQueue<T> queue = new LinkedBlockingQueue<>();

        queue(queue, o, clazz, true);

        return queue;
    }

    /**
     * Cast an object into a typed array blocking queue.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @param capacity
     *            The initial capacity
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> BlockingQueue<T> getArrayBlockingQueue(final Object o, final Class<T> clazz, final int capacity) {
        final BlockingQueue<T> queue = new ArrayBlockingQueue<>(capacity);

        queue(queue, o, clazz, true);

        return queue;
    }

    /**
     * Cast an object into a typed priority blocking queue.
     * 
     * @param o
     *            The input object
     * @param clazz
     *            The output class
     * @return The casted object
     * @param <T>
     *            The type of the output
     */
    public static <T> BlockingQueue<T> getPriorityBlockingQueue(final Object o, final Class<T> clazz) {
        final BlockingQueue<T> queue = new PriorityBlockingQueue<>();

        queue(queue, o, clazz, true);

        return queue;
    }

    /**
     * Cast an object into a typed hash map.
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

        map(map, o, classKey, classValue, false);

        return map;
    }

    /**
     * Cast an object into a typed tree map.
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
    public static <K, V> Map<K, V> getTreeMap(final Object o, final Class<K> classKey, final Class<V> classValue) {
        final Map<K, V> map = new TreeMap<>();

        map(map, o, classKey, classValue, true);

        return map;
    }

    /**
     * Cast an object into a typed tree map. Comparator has to be null safe.
     * 
     * @param o
     *            The input object
     * @param classKey
     *            The class of the key
     * @param classValue
     *            The class of the value
     * @param comparator
     *            The tree map comparator
     * @return The casted object
     * @param <K>
     *            The type of key
     * @param <V>
     *            The type of the value
     */
    public static <K, V> SortedMap<K, V> getTreeMap(final Object o, final Class<K> classKey, final Class<V> classValue,
            final Comparator<K> comparator) {
        final SortedMap<K, V> map = new TreeMap<>(comparator);

        map(map, o, classKey, classValue, false);

        return map;
    }

    /**
     * Cast an object into a typed hashtable.
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
    public static <K, V> Map<K, V> getHashtable(final Object o, final Class<K> classKey, final Class<V> classValue) {
        final Map<K, V> map = new Hashtable<>();

        map(map, o, classKey, classValue, true);

        return map;
    }

    /**
     * Cast an object into a typed hash set.
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

        set(set, o, classElement);

        return set;
    }

    /**
     * Cast an object into a typed hash set.
     * 
     * @param o
     *            The input object
     * @param classElement
     *            The class of the element in the set
     * @return The casted object
     * @param <T>
     *            The type of the element
     */
    public static <T> Set<T> getTreeSet(final Object o, final Class<T> classElement) {
        final Set<T> set = new TreeSet<>();

        set(set, o, classElement);

        return set;
    }

    /**
     * Cast an object into a typed hash set.
     * 
     * @param o
     *            The input object
     * @param classElement
     *            The class of the element in the set
     * @param comparator
     *            The comparator to keep the sort
     * @return The casted object
     * @param <T>
     *            The type of the element
     */
    public static <T> SortedSet<T> getTreeSet(final Object o, final Class<T> classElement, final Comparator<T> comparator) {
        final SortedSet<T> set = new TreeSet<>(comparator);

        set(set, o, classElement);

        return set;
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
     * @param removeNull
     *            If null has to be removed
     * @param <K>
     *            The type of key
     * @param <V>
     *            The type of the value
     * 
     */
    private static <K, V> void setMapValue(final Map<K, V> map, final K key, final Entry<?, ?> obj, final Class<V> classValue,
            final boolean removeNull) {
        if (obj.getValue() != null && classValue.isAssignableFrom(obj.getValue().getClass())) {
            map.put(key, classValue.cast(obj.getValue()));
        } else if (obj.getValue() == null && !removeNull) {
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
     * Set the queue value.
     * 
     * @param queue
     *            the queue
     * @param obj
     *            the object
     * @param clazz
     *            the class
     * @param removeNull
     *            If null has to be removed
     * @param <T>
     *            The type of the element
     */
    private static <T> void setQueueValue(final Queue<T> queue, final Object obj, final Class<T> clazz, final boolean removeNull) {
        if (obj != null && clazz.isAssignableFrom(obj.getClass())) {
            queue.add(clazz.cast(obj));
        } else if (obj == null && !removeNull) {
            queue.add(null);
        }
    }

    /**
     * Cast the object and feel the map.
     * 
     * @param map
     *            The output map
     * @param o
     *            The input object
     * @param classKey
     *            The class of the map key
     * @param classValue
     *            The class of the map value
     * @param removeNull
     *            If null has to be removed
     */
    private static <K, V> void map(final Map<K, V> map, final Object o, final Class<K> classKey, final Class<V> classValue,
            final boolean removeNull) {
        if (o != null && Map.class.isAssignableFrom(o.getClass())) {
            Map<?, ?> mObj = (Map<?, ?>) o;
            for (Entry<?, ?> obj : mObj.entrySet()) {
                if (obj.getKey() != null && classKey.isAssignableFrom(obj.getKey().getClass())) {
                    final K key = classKey.cast(obj.getKey());
                    setMapValue(map, key, obj, classValue, removeNull);
                } else if (obj.getKey() == null && !removeNull) {
                    setMapValue(map, null, obj, classValue, removeNull);
                }
            }
        }
    }

    /**
     * Cast the object and feel the queue.
     * 
     * @param queue
     *            The output queue
     * @param o
     *            The input object
     * @param classElement
     *            The typed class
     * @param removeNull
     *            If null has to be removed
     */
    private static <T> void queue(final Queue<T> queue, final Object o, final Class<T> clazz, final boolean removeNull) {
        if (o != null && Queue.class.isAssignableFrom(o.getClass())) {
            Queue<?> mObj = (Queue<?>) o;
            for (Object obj : mObj) {
                setQueueValue(queue, obj, clazz, removeNull);
            }
        }
    }

    /**
     * Cast the object and feel the list.
     * 
     * @param list
     *            The output list
     * @param o
     *            The input object
     * @param classElement
     *            The typed class
     */
    private static <T> void list(final List<T> list, final Object o, final Class<T> clazz) {
        if (o != null && List.class.isAssignableFrom(o.getClass())) {
            List<?> mObj = (List<?>) o;
            for (Object obj : mObj) {
                setListValue(list, obj, clazz);
            }
        }
    }

    /**
     * Cast the object and feel the set.
     * 
     * @param set
     *            The output set
     * @param o
     *            The input object
     * @param classElement
     *            The typed class
     */
    private static <T> void set(final Set<T> set, final Object o, final Class<T> classElement) {
        if (o != null && Set.class.isAssignableFrom(o.getClass())) {
            Set<?> mObj = (Set<?>) o;
            for (Object obj : mObj) {
                if (obj != null && classElement.isAssignableFrom(obj.getClass())) {
                    final T element = classElement.cast(obj);
                    set.add(element);
                }
            }
        }
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
     * Instantiate some objects.
     * 
     * @param logLevelInfo
     *            log level info
     * @param constructor
     *            constructor
     * @param instantiableClass
     *            instantiable class
     * @param objects
     *            list of objects
     * @return the instantiated class
     * @param <T>
     *            The type of the element
     */
    @SuppressWarnings("unchecked")
    private static <T> T instantiate(final boolean logLevelInfo, final Constructor<?> constructor, final Class<T> instantiableClass,
            final Object... objects) {
        T result = null;
        Object resultTmp;
        try {
            if (constructor != null) {
                resultTmp = constructor.newInstance(objects);
                if (resultTmp != null && instantiableClass.isAssignableFrom(resultTmp.getClass())) {
                    result = (T) resultTmp;
                }
            }
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            if (logLevelInfo) {
                LOGGER.info("Cannot instantiate " + constructor.getName());
            } else {
                LOGGER.error("Cannot instantiate " + constructor.getName());
            }
        }
        return result;
    }

    /**
     * Instantiate a typed constructor.
     * 
     * @param instantiableClass
     *            instantiable class
     * @param classes
     *            classes
     * @param objects
     *            objects
     * @return the instantiated constructor
     * @param <T>
     *            The type of the element
     */
    private static <T> T instantiateTypedConstructor(final Class<T> instantiableClass, final List<Class<?>> classes,
            final Object[] objects) {
        T result = null;

        Constructor<T> typedConstructor;
        try {
            typedConstructor = instantiableClass.getDeclaredConstructor(classes.toArray(new Class<?>[classes.size()]));

            result = instantiate(true, typedConstructor, instantiableClass, objects);
        } catch (NoSuchMethodException | SecurityException e) {
            LOGGER.info("Cannot map [" + StringUtils.join(objects, ", ") + "] into " + instantiableClass.getName(), e);
        }
        return result;
    }

    /**
     * Instantiate an unknown constructor.
     * 
     * @param instantiableClass
     *            instantiable class
     * @param classes
     *            classes
     * @param objects
     *            objects
     * @return the instantiated constructor
     * @param <T>
     *            The type of the element
     */
    private static <T> T instantiateUnknownConstructor(final Class<T> instantiableClass, final List<Class<?>> classes,
            final Object[] objects) {
        T result = null;

        try {
            Constructor<?>[] constructors = instantiableClass.getDeclaredConstructors();
            boolean mismatch = true;

            for (Constructor<?> constructor : constructors) {
                Class<?>[] parameterTypes = constructor.getParameterTypes();
                if (parameterTypes.length == classes.size()) {
                    mismatch = false;
                    for (int i = 0; i < parameterTypes.length; i++) {
                        mismatch |= classes.get(i) != null && !parameterTypes[i].isAssignableFrom(classes.get(i));
                    }
                    if (!mismatch) {
                        result = instantiate(false, constructor, instantiableClass, objects);
                        break;
                    }
                }
            }
        } catch (SecurityException e) {
            LOGGER.error("Cannot map [" + StringUtils.join(objects, ", ") + "] into " + instantiableClass.getName(), e);
        }

        return result;
    }

    /**
     * Map a list of object into a instantiable.
     * 
     * @param instantiableClass
     *            The instantiable
     * @param objects
     *            The objects to pass through the constructor
     * @return The new instance
     * @param <T>
     *            The result class
     */
    public static <T> T map(final Class<T> instantiableClass, final Object... objects) {
        List<Class<?>> classes = new ArrayList<>();
        T result = null;

        if (instantiableClass != null) {
            for (Object object : objects) {
                if (object != null) {
                    classes.add(object.getClass());
                } else {
                    classes.add(null);
                }
            }

            result = instantiateTypedConstructor(instantiableClass, classes, objects);

            if (result == null) {
                result = instantiateUnknownConstructor(instantiableClass, classes, objects);
            }
        }

        return result;
    }

    /**
     * Get the generic collection class.
     * 
     * @param instantiableClass
     *            The item class
     * @param <T>
     *            The item type
     * @return The iterable typed
     */
    @SuppressWarnings("unchecked")
    public static <T> Class<List<T>> getListTypedClass(final Class<T> instantiableClass) {
        final List<T> list = new ArrayList<>();

        try {
            list.add(instantiableClass.newInstance());
        } catch (InstantiationException | IllegalAccessException e) {
            LOGGER.error("Errors occurred in CastGenerics#getListTypedClass()", e);
        }

        return (Class<List<T>>) list.getClass();
    }
}
