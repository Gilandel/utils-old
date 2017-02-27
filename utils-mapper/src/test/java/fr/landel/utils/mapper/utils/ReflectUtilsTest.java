/*
 * #%L
 * utils-mapper
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.mapper.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;

import org.apache.commons.collections.buffer.UnboundedFifoBuffer;
import org.junit.Before;
import org.junit.Test;

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.assertor.expect.Expect;
import fr.landel.utils.commons.CastUtils;
import fr.landel.utils.mapper.MapperException;

/**
 * Check reflection util classes
 *
 * @since May 14, 2016
 * @author Gilles
 *
 */
public class ReflectUtilsTest {

    private ReflectUtils ru;

    @Before
    public void init() {
        this.ru = new ReflectUtils();
    }

    /**
     * Test method for {@link ReflectUtils#getPropertyUtilsBean()}.
     */
    @Test
    public void testGetPropertyUtilsBean() {
        assertNotNull(this.ru.getPropertyUtilsBean());
    }

    /**
     * Test method for {@link ReflectUtils#getAllFields(java.lang.Class)}.
     */
    @Test
    public void testGetAllFieldsOK() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        assertNotNull(fields);
        assertEquals(fields.size(), 10);
    }

    /**
     * Test method for {@link ReflectUtils#getAllFields(java.lang.Class)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetAllFieldsKO() {
        this.ru.getAllFields(null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test
    public void testIsGettableOK() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO obj = new ReflectDTO();

        assertFalse(this.ru.isGettable(fields.get("longNumber"), obj));
        assertTrue(this.ru.isGettable(fields.get("bool"), obj));
    }

    /**
     * Test method for
     * {@link ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsGettableKO1() {
        this.ru.isGettable(null, new ReflectDTO());
    }

    /**
     * Test method for
     * {@link ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsGettableKO2() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.isGettable(fields.get("bool"), null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test
    public void testIsSettableOK() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();

        assertFalse(this.ru.isSettable(fields.get("doubleNumber"), dto));
        assertTrue(this.ru.isSettable(fields.get("longVolatile"), dto));
    }

    /**
     * Test method for
     * {@link ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsSettableKO1() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.isSettable(fields.get("doubleNumber"), null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsSettableKO2() {
        ReflectDTO dto = new ReflectDTO();

        this.ru.isSettable(null, dto);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On Mapping error
     */
    @Test
    public void testInvokeGetterFieldSFunctionThrowableOfObjectObjectEOK() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();

        assertEquals("nullSuffix", this.ru.invokeGetter(fields.get("string"), dto, (o -> String.valueOf(o) + "Suffix")));
        assertNull(this.ru.invokeGetter(fields.get("string"), dto, null));
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On Mapping error
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeGetterFieldSFunctionThrowableOfObjectObjectEKO1() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.invokeGetter(fields.get("string"), null, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On Mapping error
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeGetterFieldSFunctionThrowableOfObjectObjectEKO2() throws MapperException {
        ReflectDTO dto = new ReflectDTO();

        this.ru.invokeGetter(null, dto, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On mapping exception
     */
    @Test
    public void testInvokeGetterFieldSClassOfRFunctionThrowableOfObjectObjectAbstractExceptionOK() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();

        assertEquals("nullSuffix", this.ru.invokeGetter(fields.get("string"), dto, String.class, (o -> String.valueOf(o) + "Suffix")));
        assertNull(this.ru.invokeGetter(fields.get("string"), dto, String.class, null));
        // Process long value to boolean
        assertFalse(this.ru.invokeGetter(fields.get("longVolatile"), dto, Boolean.class, (o -> (long) o > 0)));
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeGetterFieldSClassOfRFunctionThrowableOfObjectObjectAbstractExceptionKO1() throws MapperException {
        ReflectDTO dto = new ReflectDTO();

        this.ru.invokeGetter(null, dto, Boolean.class, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeGetterFieldSClassOfRFunctionThrowableOfObjectObjectAbstractExceptionKO2() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();

        this.ru.invokeGetter(fields.get("bool"), dto, null, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
     * .
     * 
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeGetterFieldSClassOfRFunctionThrowableOfObjectObjectAbstractExceptionKO3() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.invokeGetter(fields.get("bool"), null, Boolean.class, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
     * .
     * 
     * @throws MapperException
     *             On mapping error
     */
    @Test
    public void testInvokeSetterOK() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();
        this.ru.invokeSetter(fields.get("longVolatile"), dto, 12);

        assertEquals(12, dto.getLongVolatile());

        this.ru.invokeSetter(fields.get("object"), dto, null);

        assertNull(dto.getObject());
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
     * .
     * 
     * @throws MapperException
     *             On mapping error
     */
    @Test(expected = MapperException.class)
    public void testInvokeSetterKO1() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();
        // cast problem
        this.ru.invokeSetter(fields.get("longVolatile"), dto, "test");
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
     * .
     * 
     * @throws MapperException
     *             On mapping error
     */
    @Test(expected = MapperException.class)
    public void testInvokeSetterKO2() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        ReflectDTO dto = new ReflectDTO();
        // field is final
        this.ru.invokeSetter(fields.get("string"), dto, "test");
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
     * .
     * 
     * @throws MapperException
     *             On mapping error
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeSetterKO3() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.invokeSetter(fields.get("string"), null, "test");
    }

    /**
     * Test method for
     * {@link ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
     * .
     * 
     * @throws MapperException
     *             On mapping error
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeSetterKO4() throws MapperException {
        ReflectDTO dto = new ReflectDTO();
        this.ru.invokeSetter(null, dto, "test");
    }

    /**
     * Test method for
     * {@link ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
     * .
     * 
     * @throws SecurityException
     *             On error loading method
     * @throws NoSuchMethodException
     *             On error loading method
     * @throws MapperException
     *             On mapping exception
     */
    @Test
    public void testInvokeOK() throws NoSuchMethodException, SecurityException, MapperException {
        Method methodSet = ReflectDTO.class.getDeclaredMethod("setObject", Object.class);
        Method methodGet = ReflectDTO.class.getDeclaredMethod("getObject");

        assertNotNull(methodSet);
        assertNotNull(methodGet);

        ReflectDTO dto = new ReflectDTO();
        assertEquals("", dto.getObject());

        this.ru.invoke(methodSet, dto, "Test");

        assertEquals("Test", this.ru.invoke(methodGet, dto));
    }

    /**
     * Test method for
     * {@link ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
     * .
     * 
     * @throws SecurityException
     *             On error loading method
     * @throws NoSuchMethodException
     *             On error loading method
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = MapperException.class)
    public void testInvokeKO1() throws NoSuchMethodException, SecurityException, MapperException {
        Method methodSet = ReflectDTO.class.getDeclaredMethod("setObject", Object.class);

        assertNotNull(methodSet);

        ReflectDTO dto = new ReflectDTO();

        this.ru.invoke(methodSet, dto);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
     * .
     * 
     * @throws SecurityException
     *             On error loading method
     * @throws NoSuchMethodException
     *             On error loading method
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeKO2() throws NoSuchMethodException, SecurityException, MapperException {
        ReflectDTO dto = new ReflectDTO();

        this.ru.invoke(null, dto);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
     * .
     * 
     * @throws SecurityException
     *             On error loading method
     * @throws NoSuchMethodException
     *             On error loading method
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeKO3() throws NoSuchMethodException, SecurityException, MapperException {
        Method methodSet = ReflectDTO.class.getDeclaredMethod("setObject", Object.class);

        assertNotNull(methodSet);

        this.ru.invoke(methodSet, null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
     * .
     * 
     * @throws SecurityException
     *             On error loading method
     * @throws NoSuchMethodException
     *             On error loading method
     * @throws MapperException
     *             On mapping exception
     */
    @Test(expected = IllegalArgumentException.class)
    public void testInvokeKO4() throws NoSuchMethodException, SecurityException, MapperException {
        Method methodSet = ReflectDTO.class.getDeclaredMethod("setObject", Object.class);

        assertNotNull(methodSet);

        ReflectDTO dto = new ReflectDTO();

        this.ru.invoke(methodSet, dto, (Object[]) null);
    }

    /**
     * Test method for {@link ReflectUtils#isMappableClass(java.lang.Class)}.
     */
    @Test
    public void testIsMappableClassOK() {
        assertTrue(this.ru.isMappableClass(ReflectDTO.class));
        assertFalse(this.ru.isMappableClass(Class.class));
    }

    /**
     * Test method for {@link ReflectUtils#isMappableClass(java.lang.Class)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsMappableClassKO() {
        assertTrue(this.ru.isMappableClass(null));
    }

    /**
     * Test method for
     * {@link ReflectUtils#isMappableField(java.lang.reflect.Field)}.
     */
    @Test
    public void testIsMappableField() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        assertTrue(this.ru.isMappableField(fields.get("bool")));
        assertTrue(this.ru.isMappableField(fields.get("object")));
        assertTrue(this.ru.isMappableField(fields.get("longVolatile")));
        assertFalse(this.ru.isMappableField(fields.get("longNumber")));
    }

    /**
     * Test method for
     * {@link ReflectUtils#isMappableField(java.lang.reflect.Field)}.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsMappableFieldKO() {
        this.ru.isMappableField(null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#getClassesFromObject(java.lang.Class)}.
     */
    @Test
    public void testGetClassesFromObject() {
        Class<?>[] classes = this.ru.getClassesFromObject(ReflectDTO.class);

        assertNotNull(classes);

        Assertor.that(classes).containsAll(new Class<?>[] {Observable.class, Collection.class}).toThrow();
    }

    /**
     * Test method for {@link ReflectUtils#newInstance(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test
    public void testNewInstance() throws MapperException {
        Observable obs = this.ru.newInstance(Observable.class);
        assertNotNull(obs);
        Assertor.that(obs).isInstanceOf(Observable.class);

        Expect.exception(() -> {
            this.ru.newInstance(null);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            this.ru.newInstance(List.class);
        }, MapperException.class);
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceCollection(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test
    public void testNewInstanceCollectionOK() throws MapperException {

        Observable obs = new Observable();

        this.checkNewCollectionInstance(obs, List.class, ArrayList.class);
        this.checkNewCollectionInstance(obs, Set.class, HashSet.class);
        this.checkNewCollectionInstance(obs, Queue.class, LinkedList.class);
        this.checkNewCollectionInstance(obs, List.class, ArrayList.class);
        this.checkNewCollectionInstance(obs, new ArrayList<Observable>(), ArrayList.class);
        this.checkNewCollectionInstance(obs, new LinkedList<Observable>(), LinkedList.class);
        this.checkNewCollectionInstance(obs, new LinkedBlockingQueue<Observable>(), LinkedBlockingQueue.class);
        this.checkNewCollectionInstance("", new PriorityBlockingQueue<String>(), PriorityBlockingQueue.class); // Comparable
        this.checkNewCollectionInstance(obs, new PriorityQueue<Observable>(), PriorityQueue.class);
        this.checkNewCollectionInstance(obs, new Vector<Observable>(), Vector.class);
        this.checkNewCollectionInstance("", new TreeSet<String>(), TreeSet.class); // Comparable
        this.checkNewCollectionInstance(obs, new HashSet<Observable>(), HashSet.class);

        @SuppressWarnings("unchecked")
        List<String> list = this.ru.newInstanceCollection(MyList.class, List.class, String.class);
        assertNotNull(obs);
        Assertor.that(list).isAssignableFrom(ArrayList.class);

        Collection<?> myCollection = new UnboundedFifoBuffer(1);

        Expect.exception(() -> {
            this.ru.newInstanceCollection(CastUtils.getClass(myCollection));
        }, MapperException.class);
    }

    private <X> void checkNewCollectionInstance(final X newObject, final Class<?> collection, final Class<?> expectedClass)
            throws MapperException {
        Collection<X> obs;
        obs = this.ru.newInstanceCollection(collection, CastUtils.getClass(newObject));
        assertNotNull(obs);
        Assertor.that(obs).isAssignableFrom(expectedClass);
        obs.add(newObject);
        Assertor.that(obs).isNotEmpty();
    }

    private <X, O extends Collection<X>> void checkNewCollectionInstance(final X newObject, final Collection<X> collection,
            final Class<?> expectedClass) throws MapperException {
        this.checkNewCollectionInstance(newObject, collection, null, expectedClass);
    }

    private <X, O extends Collection<X>> void checkNewCollectionInstance(final X newObject, final Collection<X> collection,
            final Class<O> outputType, final Class<?> expectedClass) throws MapperException {
        Collection<X> obs;
        if (outputType == null) {
            obs = this.ru.newInstanceCollection(CastUtils.getClass(collection));
        } else {
            obs = this.ru.newInstanceCollection(CastUtils.getClass(collection), outputType, null);
        }
        assertNotNull(obs);
        Assertor.that(obs).isAssignableFrom(expectedClass);
        obs.add(newObject);
        Assertor.that(obs).isNotEmpty();
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceCollection(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test(expected = IllegalArgumentException.class)
    public void testNewInstanceCollectionKO1() throws MapperException {
        this.ru.newInstanceCollection(null);
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceCollection(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test(expected = ClassCastException.class)
    public void testNewInstanceCollectionKO2() throws MapperException {
        TreeSet<Observable> instanciable = new TreeSet<>();

        Set<Observable> o = instanciable;

        Set<Observable> obs = this.ru.newInstanceCollection(CastUtils.getClass(instanciable), CastUtils.getClass(o), null);

        obs.add(new Observable()); // TreeSet requires Comparable
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceCollection(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test(expected = MapperException.class)
    public void testNewInstanceCollectionKO3() throws MapperException {
        ArrayBlockingQueue<String> abq = new ArrayBlockingQueue<>(1);
        this.checkNewCollectionInstance("", abq, CastUtils.getClass((BlockingQueue<String>) abq), LinkedBlockingQueue.class);
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceMap(java.lang.Class, java.lang.Class, java.lang.Class)}
     * .
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test
    public void testNewInstanceMapClassOfQClassOfKClassOfV() throws MapperException {
        Map<String, String> map = this.ru.newInstanceMap(HashMap.class, String.class, String.class);
        assertNotNull(map);
        Assertor.that(map).isAssignableFrom(HashMap.class);
        map.put("k", "v");
        Assertor.that(map).isNotEmpty();
    }

    /**
     * Test method for
     * {@link ReflectUtils#newInstanceMap(java.lang.Class, java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test
    public void testNewInstanceMapClassOfQClassOfV() throws MapperException {
        Map<String, Object> map = this.ru.newInstanceMap(HashMap.class, String.class);
        assertNotNull(map);
        Assertor.that(map).isAssignableFrom(HashMap.class);
        map.put("k", "v");
        Assertor.that(map).isNotEmpty();
    }

    /**
     * Test method for {@link ReflectUtils#newInstanceMap(java.lang.Class)}.
     * 
     * @throws MapperException
     *             On instance creation error
     */
    @Test
    public void testNewInstanceMapClassOfQ() throws MapperException {
        Map<Object, Object> map = this.ru.newInstanceMap(HashMap.class, Object.class);
        assertNotNull(map);
        Assertor.that(map).isAssignableFrom(HashMap.class);
        map.put("k", "v");
        Assertor.that(map).isNotEmpty();

        map = this.ru.newInstanceMap(TreeMap.class, Object.class);
        assertNotNull(map);
        Assertor.that(map).isAssignableFrom(TreeMap.class);
        map.put("k", "v");
        Assertor.that(map).isNotEmpty();

        map = this.ru.newInstanceMap(Hashtable.class, Object.class);
        assertNotNull(map);
        Assertor.that(map).isAssignableFrom(Hashtable.class);
        map.put("k", "v");
        Assertor.that(map).isNotEmpty();

        Expect.exception(() -> {
            this.ru.newInstanceMap(null);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            Map<Observable, String> map1 = this.ru.newInstanceMap(TreeMap.class, Observable.class, String.class);

            map1.put(new Observable(), "");
        }, ClassCastException.class);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectionUtil#getMethod(java.lang.Class,
     * java.lang.String, java.lang.Class<?>[])}.
     * 
     * @throws MapperException
     *             On failures
     */
    @Test
    public void testGetMethod() throws MapperException {
        assertNotNull(this.ru.getMethod(ReflectDTO.class, "setBool", Boolean.TYPE));

        Expect.exception(() -> {
            this.ru.getMethod(ReflectDTO.class, "setBool");
        }, MapperException.class);

        Expect.exception(() -> {
            this.ru.getMethod(ReflectDTO.class, null);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            this.ru.getMethod(null, "setBool");
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            this.ru.getMethod(ReflectDTO.class, "setBool", (Class<?>) null);
        }, IllegalArgumentException.class);

        Expect.exception(() -> {
            this.ru.getMethod(ReflectDTO.class, "setBool", (Class<?>[]) null);
        }, IllegalArgumentException.class);
    }

    /**
     * Test method for
     * {@link ReflectUtils#getGetterMethod(java.lang.reflect.Field)}.
     * 
     * @throws MapperException
     *             On failures
     */
    @Test
    public void testGetGetterMethod() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        assertNotNull(this.ru.getGetterMethod(fields.get("bool")));

        Expect.exception(() -> {
            this.ru.getGetterMethod(fields.get("longNumber"));
        }, MapperException.class);

        Expect.exception(() -> {
            this.ru.getGetterMethod(null);
        }, IllegalArgumentException.class);
    }

    /**
     * Test method for
     * {@link ReflectUtils#getSetterMethod(java.lang.reflect.Field)}.
     * 
     * @throws MapperException
     *             On failures
     */
    @Test
    public void testGetSetterMethod() throws MapperException {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        assertNotNull(this.ru.getSetterMethod(fields.get("bool")));

        Expect.exception(() -> {
            this.ru.getSetterMethod(fields.get("longNumber"));
        }, MapperException.class);

        Expect.exception(() -> {
            this.ru.getSetterMethod(null);
        }, IllegalArgumentException.class);
    }

    private static class MyList extends ArrayList<String> {

        /**
         * serialVersionUID
         */
        private static final long serialVersionUID = -2923027114545209843L;
    }
}
