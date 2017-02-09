/*-
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.dialect;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Before;
import org.junit.Test;

/**
 * Check {@link ImprovedHSQLDialect}
 *
 * @since Aug 12, 2016
 * @author Gilles
 *
 */
public class ImprovedHSQLDialectTest {

    private ImprovedHSQLDialect dialect;

    @Before
    public void init() {
        this.dialect = new ImprovedHSQLDialect();
    }

    /**
     * Test method for {@link ImprovedHSQLDialect#dropConstraints()}.
     */
    @Test
    public void testDropConstraints() {
        assertFalse(this.dialect.dropConstraints());
    }

    /**
     * Test method for
     * {@link ImprovedHSQLDialect#supportsIfExistsBeforeTableName()}.
     */
    @Test
    public void testSupportsIfExistsBeforeTableName() {
        assertTrue(this.dialect.supportsIfExistsBeforeTableName());
    }

    /**
     * Test method for
     * {@link ImprovedHSQLDialect#supportsIfExistsAfterTableName()}.
     */
    @Test
    public void testSupportsIfExistsAfterTableName() {
        assertFalse(this.dialect.supportsIfExistsAfterTableName());
    }

    /**
     * Test method for
     * {@link ImprovedHSQLDialect#getDropSequenceString(java.lang.String)}.
     */
    @Test
    public void testGetDropSequenceStringString() {
        assertEquals("drop sequence if exists sequence if exists", this.dialect.getDropSequenceString("sequence"));
    }

    /**
     * Test method for
     * {@link ImprovedHSQLDialect#getCascadeConstraintsString()}.
     */
    @Test
    public void testGetCascadeConstraintsString() {
        assertEquals(" cascade ", this.dialect.getCascadeConstraintsString());
    }
}
