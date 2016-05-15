package fr.landel.utils.model.mapper.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import fr.landel.utils.commons.asserts.AssertUtils;
import fr.landel.utils.model.mapper.MapperException;

/**
 * Check reflection util classes
 *
 * @since 14 mai 2016
 * @author Gilles
 *
 */
public class ReflectionUtilTest {

    // private static final Logger LOGGER =
    // LoggerFactory.getLogger(ReflectionUtilTest.class);

    private ReflectUtils ru;

    @Before
    public void init() {
        this.ru = new ReflectUtils();
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getPropertyUtilsBean()}
     * .
     */
    @Test
    public void testGetPropertyUtilsBean() {
        assertNotNull(this.ru.getPropertyUtilsBean());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getAllFields(java.lang.Class)}
     * .
     */
    @Test
    public void testGetAllFieldsOK() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        assertNotNull(fields);
        assertEquals(fields.size(), 10);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getAllFields(java.lang.Class)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testGetAllFieldsKO() {
        this.ru.getAllFields(null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsGettableKO1() {
        this.ru.isGettable(null, new ReflectDTO());
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isGettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsGettableKO2() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.isGettable(fields.get("bool"), null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsSettableKO1() {
        final Map<String, Field> fields = this.ru.getAllFields(ReflectDTO.class);

        this.ru.isSettable(fields.get("doubleNumber"), null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isSettable(java.lang.reflect.Field, java.lang.Object)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsSettableKO2() {
        ReflectDTO dto = new ReflectDTO();

        this.ru.isSettable(null, dto);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeGetter(java.lang.reflect.Field, java.lang.Object, java.lang.Class, fr.landel.utils.commons.stream.FunctionThrowable)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invokeSetter(java.lang.reflect.Field, java.lang.Object, java.lang.Object)}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#invoke(java.lang.reflect.Method, java.lang.Object, java.lang.Object[])}
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
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isMappableClass(java.lang.Class)}
     * .
     */
    @Test
    public void testIsMappableClassOK() {
        assertTrue(this.ru.isMappableClass(ReflectDTO.class));
        assertFalse(this.ru.isMappableClass(Class.class));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isMappableClass(java.lang.Class)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsMappableClassKO() {
        assertTrue(this.ru.isMappableClass(null));
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isMappableField(java.lang.reflect.Field)}
     * .
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
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#isMappableField(java.lang.reflect.Field)}
     * .
     */
    @Test(expected = IllegalArgumentException.class)
    public void testIsMappableFieldKO() {
        this.ru.isMappableField(null);
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getClassesFromObject(java.lang.Class)}
     * .
     */
    @Test
    public void testGetClassesFromObject() {
        AssertUtils.contains(this.ru.getClassesFromObject(ReflectDTO.class), new Class<?>[] {Class.class, Collection.class});
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#newInstance(java.lang.Class)}
     * .
     */
    @Test
    public void testNewInstance() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#newInstanceCollection(java.lang.Class)}
     * .
     */
    @Test
    public void testNewInstanceCollection() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#newInstanceMap(java.lang.Class, java.lang.Class, java.lang.Class)}
     * .
     */
    @Test
    public void testNewInstanceMapClassOfQClassOfKClassOfV() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#newInstanceMap(java.lang.Class, java.lang.Class)}
     * .
     */
    @Test
    public void testNewInstanceMapClassOfQClassOfV() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#newInstanceMap(java.lang.Class)}
     * .
     */
    @Test
    public void testNewInstanceMapClassOfQ() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectionUtil#getMethod(java.lang.Class,
     * java.lang.String, java.lang.Class<?>[])}.
     */
    @Test
    public void testGetMethod() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getGetterMethod(java.lang.reflect.Field)}
     * .
     */
    @Test
    public void testGetGetterMethod() {
        fail("Not yet implemented");
    }

    /**
     * Test method for
     * {@link fr.landel.utils.model.mapper.utils.ReflectUtils#getSetterMethod(java.lang.reflect.Field)}
     * .
     */
    @Test
    public void testGetSetterMethod() {
        fail("Not yet implemented");
    }
}
