/*
 * #%L
 * utils-web
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.web;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Value;

/**
 * Check the resources bundle loader (in UTF-8)
 *
 * @since 11 déc. 2015
 * @author Gilles
 *
 */
public class ResourceBundleUTF8ControlTest extends AbstractTest {

    @Value("${utf8.prop1}")
    private String prop1;

    /**
     * Test method for
     * {@link ResourceBundleUTF8Control#newBundle(java.lang.String, java.util.Locale, java.lang.String, java.lang.ClassLoader, boolean)}
     * .
     */
    @Test
    public void testNewBundleStringLocaleStringClassLoaderBoolean() {
        final Control control = new ResourceBundleUTF8Control();
        ResourceBundle bundle = ResourceBundle.getBundle("test-utf8", control);

        assertEquals("texte accentu\u00e9", bundle.getString("utf8.prop1"));

        try {
            bundle = control.newBundle("test-utf8", Locale.FRANCE, "java.class", ResourceBundleUTF8Control.class.getClassLoader(), true);

            assertNotNull(bundle);

            assertEquals("texte accentu\u00e9", bundle.getString("utf8.prop1"));
        } catch (IllegalAccessException | InstantiationException | IOException e) {
            fail();
        }

        try {
            bundle = control.newBundle("test-utf8", Locale.ITALY, "java.class", ResourceBundleUTF8Control.class.getClassLoader(), true);

            assertNull(bundle);
        } catch (IllegalAccessException | InstantiationException | IOException e) {
            fail();
        }
    }

    /**
     * Test method for "utf8Properties" bean.
     */
    @Test
    public void testPropertiesFactoryBean() {
        assertEquals("texte accentu\u00e9", this.prop1);
    }
}
