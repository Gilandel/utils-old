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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.apache.commons.collections4.IterableUtils;
import org.apache.commons.collections4.Transformer;

/**
 * Utility class to manage collections.
 *
 * @since Nov 27, 2015
 * @author Gilles Landel
 *
 */
public final class CollectionUtils2 {

    /**
     * Hidden constructor.
     */
    private CollectionUtils2() {
    }

    /**
     * To array an iterable (take the type of the first element, otherwise use
     * the default 'toArray(new T[0])')
     * 
     * @param iterable
     *            The input iterable (required)
     * @param <T>
     *            The type of object in collection
     * @return The array
     */
    public static <T> T[] toArray(final Iterable<T> iterable) {
        return toArray(iterable, null);
    }

    /**
     * To array an iterable (take the type of the first element, otherwise use
     * the default 'toArray(new T[0])'). Returns {@code null} if the iterable is
     * empty.
     * 
     * @param iterable
     *            The input iterable (required)
     * @param type
     *            the type of objects (optional, if null take the type of the
     *            first element)
     * @param <T>
     *            The type of object in collection
     * @return The array
     */
    @SuppressWarnings("unchecked")
    public static <T> T[] toArray(final Iterable<T> iterable, final Class<T> type) {
        if (!IterableUtils.isEmpty(iterable)) {
            final Iterator<T> iterator = iterable.iterator();
            final List<T> list = new ArrayList<>();
            final Set<Class<T>> classes = new HashSet<>();
            while (iterator.hasNext()) {
                final T obj = iterator.next();
                list.add(obj);
                if (obj != null) {
                    classes.add(ClassUtils.getClass(obj));
                }
            }
            final Class<?> typeClass;
            if (type != null) {
                typeClass = type;
            } else if (classes.size() == 1) {
                typeClass = classes.iterator().next();
            } else {
                typeClass = Object.class;
            }
            return list.toArray((T[]) Array.newInstance(typeClass, list.size()));
        } else if (iterable != null && type != null) {
            return (T[]) Array.newInstance(type, 0);
        }
        return null;
    }

    /**
     * List all classes in an iterable ({@code null} values are excluded in the
     * output set)
     * 
     * @param iterable
     *            The iterable to check
     * @param <T>
     *            The generic type of iterable
     * @return a list of classes, even if iterable is null or empty
     */
    public static <T> Set<Class<T>> getClasses(final Iterable<T> iterable) {
        final Set<Class<T>> classes = new HashSet<>();
        if (!IterableUtils.isEmpty(iterable)) {
            final Iterator<T> iterator = iterable.iterator();
            while (iterator.hasNext()) {
                final T obj = iterator.next();
                if (obj != null) {
                    classes.add(ClassUtils.getClass(obj));
                }
            }
        }
        return classes;
    }

    /**
     * Check if the iterable contains specific types (all {@code null} values
     * and classes are excluded)
     * 
     * @param iterable
     *            The iterable to check
     * @param classes
     *            The list of classes ({@code null}, returns false, all
     *            {@code null} classes are removed)
     * @param <T>
     *            The generic type of iterable
     * @return true, if all classes are found
     */
    public static <T> boolean containsClasses(final Iterable<T> iterable, final Class<?>... classes) {
        if (classes != null && !IterableUtils.isEmpty(iterable)) {
            int found = 0;

            // remove null classes
            final Set<Class<?>> classRefSet = new HashSet<>();
            for (Class<?> clazz : classes) {
                if (clazz != null) {
                    classRefSet.add(clazz);
                }
            }

            // list all classes in provided iterable
            final Set<Class<T>> classSet = getClasses(iterable);

            // count
            for (Class<?> clazz : classRefSet) {
                if (classSet.contains(clazz)) {
                    found++;
                }
            }

            return found == classRefSet.size();
        }
        return false;
    }

    /**
     * Transform an array into an array
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted array
     */
    public static <I, O> O[] transformIntoArray(final I[] input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return CollectionUtils2.toArray(list);
    }

    /**
     * Transform an array into an array of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final I[] input) {
        return CollectionUtils2.transformIntoArray(input, false);
    }

    /**
     * Transform an array into an array of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final I[] input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        if (!list.isEmpty()) {
            return list.toArray(new String[list.size()]);
        } else {
            return null;
        }
    }

    /**
     * Transform an iterable into an array
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted array
     */
    public static <I, O> O[] transformIntoArray(final Iterable<I> input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return CollectionUtils2.toArray(list);
    }

    /**
     * Transform an iterable into an array of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final Iterable<I> input) {
        return CollectionUtils2.transformIntoArray(input, false);
    }

    /**
     * Transform an iterable into an array of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted array
     */
    public static <I> String[] transformIntoArray(final Iterable<I> input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        if (!list.isEmpty()) {
            return list.toArray(new String[list.size()]);
        } else {
            return null;
        }
    }

    /**
     * Transform an array into a list
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted list
     */
    public static <I, O> List<O> transformIntoList(final I[] input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an array into a list of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final I[] input) {
        return CollectionUtils2.transformIntoList(input, false);
    }

    /**
     * Transform an array into a list of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final I[] input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an iterable into a list
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted list
     */
    public static <I, O> List<O> transformIntoList(final Iterable<I> input, final Transformer<I, O> transformer) {
        final List<O> list = new ArrayList<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an iterable into a list of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final Iterable<I> input) {
        return CollectionUtils2.transformIntoList(input, false);
    }

    /**
     * Transform an iterable into a list of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted list
     */
    public static <I> List<String> transformIntoList(final Iterable<I> input, final boolean stringifyNull) {
        final List<String> list = new ArrayList<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an array into a set
     * 
     * @param input
     *            the input array
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted set
     */
    public static <I, O> Set<O> transformIntoSet(final I[] input, final Transformer<I, O> transformer) {
        final Set<O> list = new HashSet<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an array into a set of String (null value are kept)
     * 
     * @param input
     *            the input array
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final I[] input) {
        return CollectionUtils2.transformIntoSet(input, false);
    }

    /**
     * Transform an array into a set of String
     * 
     * @param input
     *            the input array
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final I[] input, final boolean stringifyNull) {
        final Set<String> list = new HashSet<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    /**
     * Transform an iterable into a set
     * 
     * @param input
     *            the input iterable
     * @param transformer
     *            The transformer
     * @param <I>
     *            The input type
     * @param <O>
     *            The output type
     * @return The converted set
     */
    public static <I, O> Set<O> transformIntoSet(final Iterable<I> input, final Transformer<I, O> transformer) {
        final Set<O> list = new HashSet<>();
        for (I in : input) {
            list.add(transformer.transform(in));
        }
        return list;
    }

    /**
     * Transform an iterable into a set of String (null value are kept)
     * 
     * @param input
     *            the input iterable
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final Iterable<I> input) {
        return CollectionUtils2.transformIntoSet(input, false);
    }

    /**
     * Transform an iterable into a set of String
     * 
     * @param input
     *            the input iterable
     * @param stringifyNull
     *            If null has to be stringified (like "null")
     * @param <I>
     *            The input type
     * @return The converted set
     */
    public static <I> Set<String> transformIntoSet(final Iterable<I> input, final boolean stringifyNull) {
        final Set<String> list = new HashSet<>();
        for (I in : input) {
            CollectionUtils2.addToString(list, in, stringifyNull);
        }
        return list;
    }

    private static <T> void addToString(final Collection<String> output, final T element, final boolean stringifyNull) {
        if (element != null || stringifyNull) {
            output.add(String.valueOf(element));
        } else {
            output.add(null);
        }
    }

    /**
     * Get typed list class
     * 
     * @param type
     *            The class type
     * @param <T>
     *            The type
     * @return The typed class
     */
    public static <T> Class<List<T>> getListClass(final Class<T> type) {
        return CastUtils.getClass(new ArrayList<T>());
    }

    /**
     * Get typed set class
     * 
     * @param type
     *            The class type
     * @param <T>
     *            The type
     * @return The typed class
     */
    public static <T> Class<Set<T>> getSetClass(final Class<T> type) {
        return CastUtils.getClass(new HashSet<T>());
    }

    /**
     * Get typed queue class
     * 
     * @param type
     *            The class type
     * @param <T>
     *            The type
     * @return The typed class
     */
    public static <T> Class<Queue<T>> getQueueClass(final Class<T> type) {
        return CastUtils.getClass(new LinkedList<T>());
    }

    /**
     * Get typed map class
     * 
     * @param keyType
     *            The key class
     * @param valueType
     *            The value class
     * @param <K>
     *            The key type
     * @param <V>
     *            The value type
     * @return The typed class
     */
    public static <K, V> Class<Map<K, V>> getMapClass(final Class<K> keyType, final Class<V> valueType) {
        return CastUtils.getClass(new HashMap<K, V>());
    }

    /**
     * Get the value from the map, in case of null, put the value into the map
     * at the specified key.
     * 
     * @param map
     *            The input map
     * @param key
     *            The key
     * @param defaultValue
     *            The value if no key or if value equals null
     * @param <K>
     *            The type of key
     * @param <V>
     *            The type of value
     * @return The value or the default value
     */
    public static <K, V> V getOrPut(final Map<K, V> map, final K key, final V defaultValue) {
        V value = map.get(key);

        if (value == null) {
            value = defaultValue;
            map.put(key, defaultValue);
        }

        return value;
    }
}
