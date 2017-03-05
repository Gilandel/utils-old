/*-
 * #%L
 * utils-io
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.io;

import static fr.landel.utils.io.SystemProperties.CDRAMS_DECORATIONS;
import static fr.landel.utils.io.SystemProperties.CDRAMS_PRESENTATION;
import static fr.landel.utils.io.SystemProperties.CDRAMS_REPOSITORY;
import static fr.landel.utils.io.SystemProperties.CDRAMS_VERBOSE;
import static fr.landel.utils.io.SystemProperties.COM_SUN_MIDP_IMPLEMENTATION;
import static fr.landel.utils.io.SystemProperties.COM_SUN_PACKAGE_SPEC_VERSION;
import static fr.landel.utils.io.SystemProperties.JAVA_COMPILER;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_COMMPORTS;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_CONFIGURATION;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_ENCODING;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_HOSTNAME;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_LOCALE;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_PLATFORM;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_PROFILES;
import static fr.landel.utils.io.SystemProperties.MICROEDITION_SECURERANDOM_NOFALLBACK;
import static fr.landel.utils.io.SystemProperties.SUN_CPU_ISALIST;
import static fr.landel.utils.io.SystemProperties.SUN_DESKTOP;
import static fr.landel.utils.io.SystemProperties.SUN_OS_PATCH_LEVEL;
import static fr.landel.utils.io.SystemProperties.USER_REGION;
import static fr.landel.utils.io.SystemProperties.USER_SCRIPT;
import static fr.landel.utils.io.SystemProperties.USER_VARIANT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.StringUtils;

/**
 * Check {@link SystemProperties}
 *
 * @since Mar 4, 2017
 * @author Gilles
 *
 */
public class SystemPropertiesTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(SystemPropertiesTest.class);

    private static void log(final Object object) {
        LOGGER.info(Objects.toString(object));
    }

    /**
     * Check {@link SystemProperties#getKey()}
     */
    @Test
    public void testGetKey() {
        assertEquals("os.name", SystemProperties.OS_NAME.getKey());
    }

    /**
     * Check {@link SystemProperties#getValue()}
     */
    @Test
    public void testGetValue() {
        final List<SystemProperties> excludedProperties = Arrays.asList(JAVA_COMPILER, SUN_OS_PATCH_LEVEL, USER_VARIANT, USER_SCRIPT,
                USER_REGION, SUN_DESKTOP, SUN_CPU_ISALIST, CDRAMS_DECORATIONS, CDRAMS_PRESENTATION, CDRAMS_REPOSITORY, CDRAMS_VERBOSE,
                COM_SUN_MIDP_IMPLEMENTATION, COM_SUN_PACKAGE_SPEC_VERSION, MICROEDITION_COMMPORTS, MICROEDITION_CONFIGURATION,
                MICROEDITION_ENCODING, MICROEDITION_HOSTNAME, MICROEDITION_LOCALE, MICROEDITION_PLATFORM, MICROEDITION_PROFILES,
                MICROEDITION_SECURERANDOM_NOFALLBACK);

        LOGGER.info("----- ALL KNOWN PROPS");
        Arrays.asList(SystemProperties.values()).stream().forEach(SystemPropertiesTest::log);

        LOGGER.info("----- NOT FOUND PROPS");
        Arrays.asList(SystemProperties.values()).stream().filter(p -> StringUtils.isEmpty(p.getValue())).forEach(SystemPropertiesTest::log);

        LOGGER.info("----- NEW PROPS");
        final List<String> keys = Arrays.asList(SystemProperties.values()).stream().map(p -> p.getKey()).collect(Collectors.toList());
        System.getProperties().entrySet().stream().filter(p -> !keys.contains(p.getKey())).forEach(SystemPropertiesTest::log);

        LOGGER.info("-----");

        for (SystemProperties property : SystemProperties.values()) {
            if (!excludedProperties.contains(property)) {
                assertTrue(StringUtils.inject("Property '{}' is not correct: {}", property.getKey(), property.getValue()),
                        StringUtils.isNotEmpty(property.getValue()));
            }
        }
    }
}
