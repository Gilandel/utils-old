/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper.utils;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

import org.apache.commons.beanutils.PropertyUtilsBean;
import org.apache.commons.lang3.StringUtils;

import fr.landel.utils.asserts.AssertUtils;
import fr.landel.utils.commons.exception.AbstractException;
import fr.landel.utils.commons.stream.FunctionThrowable;
import fr.landel.utils.mapper.MapperException;
import fr.landel.utils.mapper.mappable.Mappable;
import fr.landel.utils.mapper.mappable.MappableProperty;

/**
 * Utilities to map.
 *
 * @since 13 juil. 2015
 * @author Erwan Ropartz
 * @author Gilles Landel
 *
 */
public class ReflectUtils {

    private static final String ERROR_MSG = "Reflection failed";

    private static final String IS_PREFIX = "is";
    private static final String GET_PREFIX = "get";
    private static final String SET_PREFIX = "set";

    private PropertyUtilsBean pub;

    /**
     * Default constructor.
     */
    public ReflectUtils() {
        this.pub = new PropertyUtilsBean();
    }

    /**
     * Get the property utils bean.
     * 
     * @return the property utils bean
     */
    public PropertyUtilsBean getPropertyUtilsBean() {
        return this.pub;
    }

    /**
     * Get the list of all the fields in the specified class.
     * 
     * @param clazz
     *            The class (required)
     * @return the Map
     */
    public Map<String, Field> getAllFields(final Class<?> clazz) {

        AssertUtils.check(clazz).isNotNull("The class is null");

        return this.getAllFieldsRec(clazz, new HashMap<String, Field>());
    }

    /**
     * Get all fields recursively.
     * 
     * @param clazz
     *            class (required)
     * @param map
     *            map (required)
     * @return the map of fields
     */
    private Map<String, Field> getAllFieldsRec(final Class<?> clazz, final Map<String, Field> map) {
        Class<?> superClazz = clazz.getSuperclass();
        if (superClazz != null) {
            this.getAllFieldsRec(superClazz, map);
        }

        for (Field field : clazz.getDeclaredFields()) {
            if (!field.isSynthetic()) {
                map.put(field.getName(), field);
            }
        }
        return map;
    }

    /**
     * Check if getter is available and the field readable.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @return If getter is available
     */
    public <S> boolean isGettable(Field sourceField, S sourceObject) {

        AssertUtils.check(sourceField).isNotNull("The source field is null");
        AssertUtils.check(sourceObject).isNotNull("The source object is null");

        return this.pub.isReadable(sourceObject, sourceField.getName());
    }

    /**
     * Check if setter is available and field writable.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @return If setter is available
     */
    public <S> boolean isSettable(final Field sourceField, final S sourceObject) {

        AssertUtils.check(sourceField).isNotNull("The source field is null");
        AssertUtils.check(sourceObject).isNotNull("The source object is null");

        return this.pub.isWriteable(sourceObject, sourceField.getName());
    }

    /**
     * Invoke the getter.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param <S>
     *            The object source type
     * @param <E>
     *            The post action exception type
     * @param postAction
     *            Call the function before casting the object (optional)
     * @return The value
     * @throws MapperException
     *             On invoking failed
     * @throws IllegalArgumentException
     *             On null field
     */
    public <S, E extends AbstractException> Object invokeGetter(final Field sourceField, final S sourceObject,
            final FunctionThrowable<Object, Object, E> postAction) throws MapperException {

        AssertUtils.check(sourceField).isNotNull("The source field is null");
        AssertUtils.check(sourceObject).isNotNull("The source object is null");

        Object object;
        try {
            object = this.pub.getProperty(sourceObject, sourceField.getName());
            if (postAction != null) {
                return postAction.applyThrows(object);
            }
            return object;
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | AbstractException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Call the getter method from the field and the object.
     * 
     * @param field
     *            The field (required)
     * @param instance
     *            The instance (required)
     * @param resultClass
     *            The class of the result type (required)
     * @param postAction
     *            Call the function before casting the object (optional)
     * @param <S>
     *            The source type
     * @param <R>
     *            The return type
     * @return The result
     * @throws MapperException
     *             On invocation exception
     */
    public <R, S> R invokeGetter(final Field field, final S instance, final Class<R> resultClass,
            final FunctionThrowable<Object, R, AbstractException> postAction) throws MapperException {

        AssertUtils.check(field).isNotNull("The field is null");
        AssertUtils.check(instance).isNotNull("The instance is null");
        AssertUtils.check(resultClass).isNotNull("The result class is null");

        try {
            final Object object = this.pub.getProperty(instance, field.getName());
            if (postAction != null) {
                return postAction.applyThrows(object);
            } else if (object == null) {
                return null;
            } else if (resultClass.isAssignableFrom(object.getClass())) {
                return resultClass.cast(object);
            } else {
                throw new MapperException(ERROR_MSG);
            }
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException | AbstractException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Invoke the setter.
     * 
     * @param sourceField
     *            the source field (required)
     * @param sourceObject
     *            The source object (required)
     * @param value
     *            The value to set (required, can be null)
     * @param <S>
     *            The object source type
     * @throws MapperException
     *             On invoking failed
     */
    public <S> void invokeSetter(final Field sourceField, final S sourceObject, final Object value) throws MapperException {

        AssertUtils.check(sourceField).isNotNull("The source field is null");
        AssertUtils.check(sourceObject).isNotNull("The source object is null");

        try {
            this.pub.setProperty(sourceObject, sourceField.getName(), value);
        } catch (IllegalArgumentException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Invoke the method.
     * 
     * @param method
     *            the method to invoke (required)
     * @param sourceObject
     *            The source object (required)
     * @param parameters
     *            the parameters (optional, cannot be null)
     * @param <S>
     *            The object source type
     * @return The value
     * @throws MapperException
     *             On invoking failed
     */
    public <S> Object invoke(final Method method, final S sourceObject, final Object... parameters) throws MapperException {

        AssertUtils.check(method).isNotNull("The method is null");
        AssertUtils.check(sourceObject).isNotNull("The source object is null");
        AssertUtils.check(parameters).isNotNull("The parameters is null");

        try {
            return method.invoke(sourceObject, parameters);
        } catch (IllegalAccessException | InvocationTargetException | IllegalArgumentException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Check if the class is mappable.
     * 
     * @param clazz
     *            The class (required)
     * @return If the class has the mappable annotation
     */
    public Boolean isMappableClass(final Class<?> clazz) {

        AssertUtils.check(clazz).isNotNull("The class is null");

        return clazz.getAnnotation(Mappable.class) != null;
    }

    /**
     * Check if the field is mappable.
     * 
     * @param field
     *            The field (required)
     * @return If the field has the mappable annotation
     */
    public Boolean isMappableField(final Field field) {

        AssertUtils.check(field).isNotNull("The field is null");

        MappableProperty[] mappable = field.getDeclaredAnnotationsByType(MappableProperty.class);
        if (mappable != null && mappable.length > 0) {
            return true;
        }

        boolean ret = false;
        try {
            Method method = this.getGetterMethod(field);

            if (method != null) {
                mappable = method.getDeclaredAnnotationsByType(MappableProperty.class);
                ret = mappable != null && mappable.length > 0;
            }
        } catch (MapperException e) {
            // Do nothing
        }
        return ret;
    }

    /**
     * Get the type of the reverse.
     * 
     * @param mappableClass
     *            The mappable class (required)
     * @param <E>
     *            The reverse object type
     * @param <D>
     *            The object type
     * @return The class of the object
     */
    @SuppressWarnings("unchecked")
    public <D, E> Class<E>[] getClassesFromObject(final Class<D> mappableClass) {

        AssertUtils.check(mappableClass).isNotNull("The mappable class is null");

        final Mappable mappableReverse = mappableClass.getAnnotation(Mappable.class);
        if (mappableReverse != null && mappableReverse.value().length > 0) {
            return (Class<E>[]) mappableReverse.value();
        }
        return null;
    }

    /**
     * Create a new instance of the specified class.
     * 
     * @param clazz
     *            The class to instantiate (required)
     * @param <I>
     *            The class type
     * @return The new instance
     * @throws MapperException
     *             If creation failed
     */
    public <I> I newInstance(final Class<I> clazz) throws MapperException {

        AssertUtils.check(clazz).isNotNull("The class is null");

        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new MapperException(ERROR_MSG, e);
        }
    }

    /**
     * Create a new instance of a collection (List, Set or Queue).
     * 
     * @param collectionClass
     *            The class to instantiate (required)
     * @param <C>
     *            The collection type (C has to extend Collection&lt;X&gt;)
     * @param <X>
     *            The generic type
     * @return The new instance of the collection (or null if not a list, set or
     *         queue)
     * @throws MapperException
     *             On creation failed
     */
    public <C extends Collection<X>, X> C newInstanceCollection(final Class<C> collectionClass) throws MapperException {

        return newInstanceCollection(collectionClass, collectionClass, null);
    }

    /**
     * Create a new instance of a collection (List, Set or Queue).
     * 
     * @param collectionClass
     *            The class to instantiate (required)
     * @param valueClass
     *            The generic type (required)
     * @param <C>
     *            The collection type (C has to extend Collection&lt;X&gt;)
     * @param <X>
     *            The generic type
     * @return The new instance of the collection (or null if not a list, set or
     *         queue)
     * @throws MapperException
     *             On creation failed
     */
    @SuppressWarnings("unchecked")
    public <C extends Collection<X>, X> C newInstanceCollection(final Class<?> collectionClass, final Class<X> valueClass)
            throws MapperException {

        AssertUtils.check(valueClass).isNotNull("The value class is null");

        return newInstanceCollection((Class<C>) collectionClass, (Class<C>) collectionClass, valueClass);
    }

    /**
     * Create a new instance of a collection (List, Set or Queue).
     * 
     * @param instanciableCollectionClass
     *            The class to instantiate (required)
     * @param outputCollectionClass
     *            The output class, can be the same as
     *            instanciableCollectionClass or an interface (required)
     * @param valueClass
     *            The generic type (optional)
     * @param <C>
     *            The collection type (C has to be equal to O or extend O)
     * @param <O>
     *            The collection type (O has to extend Collection&lt;X&gt;)
     * @param <X>
     *            The generic type
     * @return The new instance of the collection (or null if not a list, set or
     *         queue)
     * @throws MapperException
     *             On creation failed
     */
    public <C extends Collection<X>, O extends Collection<X>, X> O newInstanceCollection(final Class<C> instanciableCollectionClass,
            final Class<O> outputCollectionClass, final Class<X> valueClass) throws MapperException {

        AssertUtils.check(outputCollectionClass).isNotNull("The output collection class is null");
        AssertUtils.check(instanciableCollectionClass).isNotNull("The instanciable collection class is null")
                .isAssignable(outputCollectionClass, "The output class is not assignable from instanciable class");

        final O collection;

        if (Queue.class.isAssignableFrom(instanciableCollectionClass)) {
            if (BlockingQueue.class.isAssignableFrom(instanciableCollectionClass)) {
                if (PriorityBlockingQueue.class.isAssignableFrom(instanciableCollectionClass)) {
                    collection = outputCollectionClass.cast(new PriorityBlockingQueue<X>());
                } else if (LinkedBlockingQueue.class.isAssignableFrom(instanciableCollectionClass)) {
                    collection = outputCollectionClass.cast(new LinkedBlockingQueue<X>());
                } else {
                    throw new MapperException("The instanciable blocking queue class is not supported");
                }
            } else if (PriorityQueue.class.isAssignableFrom(instanciableCollectionClass)) {
                collection = outputCollectionClass.cast(new PriorityQueue<X>());
            } else {
                collection = outputCollectionClass.cast(new LinkedList<X>());
            }
        } else if (List.class.isAssignableFrom(instanciableCollectionClass)) {
            if (Vector.class.isAssignableFrom(instanciableCollectionClass)) {
                collection = outputCollectionClass.cast(new Vector<X>());
            } else {
                collection = outputCollectionClass.cast(new ArrayList<X>());
            }
        } else if (Set.class.isAssignableFrom(instanciableCollectionClass)) {
            if (TreeSet.class.isAssignableFrom(instanciableCollectionClass)) {
                collection = outputCollectionClass.cast(new TreeSet<X>());
            } else {
                collection = outputCollectionClass.cast(new HashSet<X>());
            }
        } else {
            throw new MapperException("The instanciable collection class is not supported");
        }

        return collection;
    }

    /**
     * Create a new instance of a map.
     * 
     * <pre>
     * Map&lt;String, Integer&gt; map = this.ru.newInstanceMap(Map.class, String.class, Integer.class);
     * </pre>
     * 
     * @param classMap
     *            The map class (required)
     * @param classKey
     *            The key class (required)
     * @param classValue
     *            The class of the value (required)
     * @param <K>
     *            The key type
     * @param <V>
     *            The value type
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    @SuppressWarnings("unchecked")
    public <M extends Map<K, V>, K, V> M newInstanceMap(final Class<?> classMap, final Class<K> classKey, final Class<V> classValue)
            throws MapperException {

        AssertUtils.check(classKey).isNotNull("The map key class is null");
        AssertUtils.check(classValue).isNotNull("The map value class is null");

        return newInstanceMap((Class<M>) classMap);
    }

    /**
     * Create a new instance of a map (Map&lt;K, Object&gt;).
     * 
     * <pre>
     * Map&lt;String, Object&gt; map = this.ru.newInstanceMap(Map.class, String.class);
     * </pre>
     * 
     * @param classMap
     *            The map class (required)
     * @param keyClass
     *            The class of the key (required)
     * @param <K>
     *            The key type
     * @param <V>
     *            The value type
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    @SuppressWarnings("unchecked")
    public <M extends Map<K, V>, K, V> M newInstanceMap(final Class<?> classMap, final Class<K> keyClass) throws MapperException {

        return newInstanceMap(classMap, keyClass, (Class<V>) Object.class);
    }

    /**
     * Create a new instance of a map.
     * 
     * <pre>
     * Class&lt;TreeMap&lt;String, Integer&gt;&gt; clazz = CastGenerics.getClass(new TreeMap&lt;String, Integer&gt;);
     * Map&lt;String, Integer&gt; map = this.ru.newInstanceMap(clazz);
     * </pre>
     * 
     * @param classMap
     *            The map class (required)
     * @param <K>
     *            The key type
     * @param <V>
     *            The value type
     * @return The new typed map
     * @throws MapperException
     *             On creation failed
     */
    public <M extends Map<K, V>, K, V> M newInstanceMap(final Class<M> classMap) throws MapperException {

        AssertUtils.check(classMap).isNotNull("The map class is null");

        final Map<K, V> map;

        if (Map.class.isAssignableFrom(classMap)) {
            if (TreeMap.class.isAssignableFrom(classMap)) {
                map = new TreeMap<>();
            } else if (Hashtable.class.isAssignableFrom(classMap)) {
                map = new Hashtable<>();
            } else {
                map = new HashMap<>();
            }
        } else {
            map = null;
        }

        return classMap.cast(map);
    }

    /**
     * Get the method from the name and its parameter types.
     * 
     * @param clazz
     *            The class (required)
     * @param name
     *            The method name (required and cannot empty be blank)
     * @param parameterTypes
     *            The parameter types (optional, cannot be null)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getMethod(final Class<?> clazz, final String name, final Class<?>... parameterTypes) throws MapperException {

        AssertUtils.check(clazz).isNotNull("The class is null");
        AssertUtils.check(name).isNotBlank("The name is null or blank");
        AssertUtils.check(parameterTypes).isNotNull("The parameter types array is null")
                .hasNoNullElements("At least one parameter type is null");

        Method method;
        try {
            if (parameterTypes.length > 0) {
                method = clazz.getDeclaredMethod(name, parameterTypes);
            } else {
                method = clazz.getDeclaredMethod(name);
            }
        } catch (NoSuchMethodException | SecurityException e) {
            if (clazz.getSuperclass() != null) {
                method = this.getMethod(clazz.getSuperclass(), name, parameterTypes);
            } else {
                throw new MapperException(ERROR_MSG, e);
            }
        }
        return method;
    }

    /**
     * Get the getter method from the field.
     * 
     * @param field
     *            The field name (required)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getGetterMethod(final Field field) throws MapperException {

        AssertUtils.check(field).isNotNull("The field is null");

        Method method;
        String capitalizedName;

        capitalizedName = StringUtils.capitalize(field.getName());

        if (Boolean.TYPE.isAssignableFrom(field.getType())) {
            try {
                method = this.getMethod(field.getDeclaringClass(), IS_PREFIX + capitalizedName);
            } catch (MapperException e) {
                method = this.getMethod(field.getDeclaringClass(), GET_PREFIX + capitalizedName);
            }
        } else {
            method = this.getMethod(field.getDeclaringClass(), GET_PREFIX + capitalizedName);
        }

        return method;
    }

    /**
     * Get the setter method from the field.
     *
     * @param field
     *            The field name (required)
     * @return The matching method
     * @throws MapperException
     *             On reflection failed
     */
    public Method getSetterMethod(final Field field) throws MapperException {

        AssertUtils.check(field).isNotNull("The field is null");

        Method method;
        String capitalizedName;

        capitalizedName = StringUtils.capitalize(field.getName());

        method = this.getMethod(field.getDeclaringClass(), SET_PREFIX + capitalizedName, field.getType());

        return method;
    }
}
