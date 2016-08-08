package fr.landel.utils.assertor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.apache.commons.lang3.tuple.Triple;
import org.junit.Test;

/**
 * Check {@link AssertorResult}
 *
 * @since Aug 8, 2016
 * @author Gilles
 *
 */
public class AssertorResultTest extends AbstractTest {

    /**
     * Test method for
     * {@link AssertorResult#AssertorResult(java.lang.Object, EnumType)}.
     */
    @Test
    public void testAssertorResultTEnumType() {
        AssertorResult<Boolean> assertorResult = new AssertorResult<>(true, EnumType.BOOLEAN);

        assertNotNull(assertorResult);

        assertEquals(true, assertorResult.getObject());
        assertEquals(EnumType.BOOLEAN, assertorResult.getType());
        assertFalse(assertorResult.isNot());
        assertTrue(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("", assertorResult.getPreconditionMessage());
        assertEquals("", assertorResult.getMessage());
        assertNull(assertorResult.getOperator());
        assertEquals(1, assertorResult.getParameters().size());
        assertEquals(Triple.of(true, EnumType.BOOLEAN, true), assertorResult.getParameters().get(0));
    }

    /**
     * Test method for {@link AssertorResult#AssertorResult(AssertorResult)}.
     */
    @Test
    public void testAssertorResultAssertorResultOfT() {
        AssertorResult<Boolean> assertorResult = new AssertorResult<>(new AssertorResult<>(true, EnumType.BOOLEAN));

        assertNotNull(assertorResult);

        assertEquals(true, assertorResult.getObject());
        assertEquals(EnumType.BOOLEAN, assertorResult.getType());
        assertTrue(assertorResult.isNot()); // NOT
        assertTrue(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("", assertorResult.getPreconditionMessage());
        assertEquals("", assertorResult.getMessage());
        assertNull(assertorResult.getOperator());
        assertEquals(1, assertorResult.getParameters().size());
        assertEquals(Triple.of(true, EnumType.BOOLEAN, true), assertorResult.getParameters().get(0));

        assertorResult = new AssertorResult<>(assertorResult);

        assertFalse(assertorResult.isNot()); // NOT
    }

    /**
     * Test method for
     * {@link AssertorResult#AssertorResult(AssertorResult, java.lang.Object, EnumOperator)}.
     */
    @Test
    public void testAssertorResultAssertorResultOfXTEnumOperator() {
        AssertorResult<Boolean> assertorResult = new AssertorResult<>(new AssertorResult<>(true, EnumType.BOOLEAN), false, EnumOperator.OR);

        assertNotNull(assertorResult);

        assertEquals(false, assertorResult.getObject());
        assertEquals(EnumType.BOOLEAN, assertorResult.getType());
        assertFalse(assertorResult.isNot());
        assertTrue(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("", assertorResult.getPreconditionMessage());
        assertEquals("", assertorResult.getMessage());
        assertEquals(EnumOperator.OR, assertorResult.getOperator());
        assertEquals(2, assertorResult.getParameters().size());
        assertEquals(Triple.of(true, EnumType.BOOLEAN, true), assertorResult.getParameters().get(0));
        assertEquals(Triple.of(false, EnumType.BOOLEAN, true), assertorResult.getParameters().get(1));
    }

    /**
     * Test method for
     * {@link AssertorResult#AssertorResult(AssertorResult, EnumOperator)}.
     */
    @Test
    public void testAssertorResultAssertorResultOfTEnumOperator() {
        final AssertorResult<String> a = new AssertorResult<>("test", EnumType.CHAR_SEQUENCE);

        AssertorResult<String> assertorResult = new AssertorResult<>(a, EnumOperator.AND);
        assertEquals(EnumOperator.AND, assertorResult.getOperator());

        assertorResult = new AssertorResult<>(a, EnumOperator.OR);
        assertEquals(EnumOperator.OR, assertorResult.getOperator());

        assertorResult = new AssertorResult<>(a, EnumOperator.XOR);
        assertEquals(EnumOperator.XOR, assertorResult.getOperator());

        assertorResult = new AssertorResult<>(a, (EnumOperator) null);
        assertNull(assertorResult.getOperator());
        assertEquals("", assertorResult.getMessage().toString());
    }

    /**
     * Test method for
     * {@link AssertorResult#AssertorResult(AssertorResult, AssertorResult, EnumOperator)}.
     */
    @Test
    public void testAssertorResultAssertorResultOfTAssertorResultOfXEnumOperator() {
        final AssertorResult<String> a = new AssertorResult<>("test", EnumType.CHAR_SEQUENCE);
        final AssertorResult<Boolean> b = new AssertorResult<>(true, EnumType.BOOLEAN);

        // precondition: true & true, valid: true & true
        AssertorResult<String> assertorResult1 = new AssertorResult<>(a, true, true, "pre-error1", "error1");
        AssertorResult<Boolean> assertorResult2 = new AssertorResult<>(b, true, true, "pre-error2", "error2");

        AssertorResult<String> assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertTrue(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: true & false, valid: true & false
        assertorResult1 = new AssertorResult<>(a, true, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("(error2)", assertorResult.getMessage().toString());

        // precondition: false & true, valid: false & true
        assertorResult1 = new AssertorResult<>(a, false, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, true, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error1", assertorResult.getPreconditionMessage().toString());
        assertEquals("error1", assertorResult.getMessage().toString());

        // precondition: true & true, valid: false & false
        assertorResult1 = new AssertorResult<>(a, true, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, true, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.AND);

        assertTrue(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("", assertorResult.getPreconditionMessage().toString());
        assertEquals("error1 AND (error2)", assertorResult.getMessage().toString());

        // precondition: false & false, valid: false & true
        assertorResult1 = new AssertorResult<>(a, false, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.OR);

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & false
        assertorResult1 = new AssertorResult<>(a, false, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.OR);

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & true
        assertorResult1 = new AssertorResult<>(a, false, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.OR);

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & true
        assertorResult1 = new AssertorResult<>(a, false, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.OR);

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("error1 OR (error2)", assertorResult.getMessage().toString());

        // precondition: false & false, valid: false & true
        assertorResult1 = new AssertorResult<>(a, false, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.XOR);

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & false
        assertorResult1 = new AssertorResult<>(a, false, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.XOR);

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & true
        assertorResult1 = new AssertorResult<>(a, false, true, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, true, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.XOR);

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("", assertorResult.getMessage().toString());

        // precondition: false & false, valid: true & true
        assertorResult1 = new AssertorResult<>(a, false, false, "pre-error1", "error1");
        assertorResult2 = new AssertorResult<>(b, false, false, "pre-error2", "error2");

        assertorResult = new AssertorResult<>(assertorResult1, assertorResult2, EnumOperator.XOR);

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error1 AND pre-error2", assertorResult.getPreconditionMessage().toString());
        assertEquals("error1 XOR (error2)", assertorResult.getMessage().toString());
    }

    /**
     * Test method for
     * {@link AssertorResult#AssertorResult(AssertorResult, boolean, boolean, java.lang.CharSequence, java.lang.CharSequence)}.
     */
    @Test
    public void testAssertorResultAssertorResultOfTBooleanBooleanCharSequenceCharSequence() {
        // precondition: true, valid: true
        AssertorResult<String> assertorResult = new AssertorResult<>(new AssertorResult<>("test", EnumType.CHAR_SEQUENCE), true, true,
                "pre-error", "error");

        assertTrue(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error", assertorResult.getPreconditionMessage().toString());
        assertEquals("error", assertorResult.getMessage().toString());

        // precondition: false, valid: true
        assertorResult = new AssertorResult<>(new AssertorResult<>("test", EnumType.CHAR_SEQUENCE), false, true, "pre-error", "error");

        assertFalse(assertorResult.isPreconditionOK());
        assertTrue(assertorResult.isValid());
        assertEquals("pre-error", assertorResult.getPreconditionMessage().toString());
        assertEquals("error", assertorResult.getMessage().toString());

        // precondition: true, valid: false
        assertorResult = new AssertorResult<>(new AssertorResult<>("test", EnumType.CHAR_SEQUENCE), true, false, "pre-error", "error");

        assertTrue(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error", assertorResult.getPreconditionMessage().toString());
        assertEquals("error", assertorResult.getMessage().toString());

        // precondition: false, valid: false
        assertorResult = new AssertorResult<>(new AssertorResult<>("test", EnumType.CHAR_SEQUENCE), false, false, "pre-error", "error");

        assertFalse(assertorResult.isPreconditionOK());
        assertFalse(assertorResult.isValid());
        assertEquals("pre-error", assertorResult.getPreconditionMessage().toString());
        assertEquals("error", assertorResult.getMessage().toString());
    }
}
