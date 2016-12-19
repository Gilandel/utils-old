/*
 * #%L
 * utils-model
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.model.hibernate;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.hibernate.boot.model.naming.Identifier;
import org.junit.Test;

/**
 * Check ImprovedNamingStrategy
 *
 * @since 7 janv. 2016
 * @author Gilles
 *
 */
public class ImprovedNamingStrategyTest {

    /**
     * Test method for {@link ImprovedNamingStrategy#toPhysicalTableName} .
     */
    @Test
    public void testToPhysicalTableName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalTableName(Identifier.toIdentifier("testIDENTIFIER"), null);
        assertEquals("test_identifier", identifier.getText());

        identifier = ins.toPhysicalTableName(new Identifier("  \t", false), null);
        assertEquals("  \t", identifier.getText());

        identifier = ins.toPhysicalTableName(Identifier.toIdentifier(null), null);
        assertNull(identifier);
    }

    /**
     * Test method for {@link ImprovedNamingStrategy#toPhysicalCatalogName} .
     */
    @Test
    public void testToPhysicalCatalogName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalCatalogName(Identifier.toIdentifier("testIDENTIFIER"), null);
        assertEquals("test_identifier", identifier.getText());
    }

    /**
     * Test method for {@link ImprovedNamingStrategy#toPhysicalColumnName} .
     */
    @Test
    public void testToPhysicalColumnName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalColumnName(Identifier.toIdentifier("testIDENTIFIER"), null);
        assertEquals("test_identifier", identifier.getText());
    }

    /**
     * Test method for {@link ImprovedNamingStrategy#toPhysicalSchemaName} .
     */
    @Test
    public void testToPhysicalSchemaName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalSchemaName(Identifier.toIdentifier("testIDENTIFIER"), null);
        assertEquals("test_identifier", identifier.getText());
    }

    /**
     * Test method for {@link ImprovedNamingStrategy#toPhysicalSequenceName} .
     */
    @Test
    public void testToPhysicalSequenceName() {
        ImprovedNamingStrategy ins = new ImprovedNamingStrategy();

        Identifier identifier = ins.toPhysicalSequenceName(Identifier.toIdentifier("testIDENTIFIER"), null);
        assertEquals("test_identifier", identifier.getText());
    }
}
