/*
 * #%L
 * utils-scripts
 * %%
 * Copyright (C) 2016 Gilandel
 * %%
 * Authors: Gilles Landel
 * URL: https://github.com/Gilandel
 * 
 * This file is under Apache License, version 2.0 (2004).
 * #L%
 */
package fr.landel.utils.scripts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.io.FileUtils;
import fr.landel.utils.scripts.ScriptsLoader;
import fr.landel.utils.scripts.PatientSearch.Attendance;
import fr.landel.utils.scripts.PatientSearch.Distance;
import fr.landel.utils.scripts.PatientSearch.Health;
import fr.landel.utils.scripts.PatientSearch.Status;

/**
 * Check scripts loader
 *
 * @since 1 déc. 2015
 * @author Gilles
 *
 */
public class ScriptsLoaderTest {

    private static final Logger LOGGER = LoggerFactory.getLogger(ScriptsLoaderTest.class);

    private static final String PARAM_STATUS = "status";
    private static final String PARAM_ATTENDANCE = "attendance";
    private static final String PARAM_SECTOR = "sectorId";
    private static final String PARAM_DOCTOR = "doctorId";
    private static final String PARAM_UNIT = "unitId";
    private static final String PARAM_RECORD_NUMBER = "recordNumber";
    private static final String PARAM_NAME = "name";
    private static final String PARAM_FIRSTNAME = "firstName";
    private static final String PARAM_BIRTHDAY = "birthday";
    private static final String PARAM_GENDER = "gender";

    private static final Pattern PATTERN_COLUMN = Pattern.compile("\\w+");

    private static final String PATH = "src/test/resources/scripts/";

    private ScriptsLoader queriesLoader;

    /**
     * initialize the loader with scripts list
     */
    @Before
    public void init() {
        this.queriesLoader = new ScriptsLoader();
        this.queriesLoader.init(EnumScripts.values());
    }

    /**
     * Test queries loader
     */
    @Test
    public void test() {
        String key = "app.id";
        String replacement = "28";
        StringBuilder builder = this.queriesLoader.get(EnumScripts.TEST, key, replacement);
        assertNotNull(builder);
        assertEquals(-1, builder.indexOf(key));
        assertTrue(builder.indexOf(replacement) > -1);
        assertEquals(-1, builder.indexOf("{"));
        assertEquals(-1, builder.indexOf("}"));
    }

    /**
     * Test queries loader
     */
    @Test
    public void patientsSearchTest() {
        final Map<String, String> replacements = new HashMap<>();

        PatientSearch patientSearch = new PatientSearch();
        patientSearch.setName("PAT");

        Map<String, Boolean> orderBy = new HashMap<>();
        orderBy.put("lastName", true);
        orderBy.put("firstName", true);

        this.defineReplacements(patientSearch, -1, -1, orderBy);

        // replacements.put("count", Boolean.TRUE.toString());

        LOGGER.info("Call queriesLoader for patientSearch");

        StringBuilder builder = this.queriesLoader.get(EnumScripts.PATIENTS_SEARCH, replacements);

        LOGGER.info("QueriesLoader for patientSearch done");

        assertNotNull(builder);
        try {
            StringBuilder expected = FileUtils.getFileContent(PATH + "patientsSearch.expected.sql", StandardCharsets.UTF_8);

            assertEquals(expected.length(), builder.length());
            assertEquals(expected.toString(), builder.toString());
        } catch (IOException e) {
            fail("Errors occurred in ScriptsLoaderTest#patientsSearchTest()");
        }
    }

    private Map<String, String> defineReplacements(final PatientSearch patientMultiSearch, final int firstResult, final int maxResults,
            final Map<String, Boolean> orderByAUB) {
        final Map<String, String> replacements = new HashMap<>();

        if (firstResult > -1 && maxResults > -1) {
            replacements.put("firstResult", String.valueOf(firstResult));
            replacements.put("maxResults", String.valueOf(maxResults));
        }

        this.manageVacation(replacements, patientMultiSearch);
        this.manageUnit(replacements, patientMultiSearch);
        this.manageStatus(replacements, patientMultiSearch);
        this.manageDoctor(replacements, patientMultiSearch);
        this.manageRecordNumber(replacements, patientMultiSearch);
        this.manageLastname(replacements, patientMultiSearch);
        this.manageFirstname(replacements, patientMultiSearch);
        this.manageBirthday(replacements, patientMultiSearch);
        this.manageGender(replacements, patientMultiSearch);
        this.manageAttendance(replacements, patientMultiSearch);

        this.manageSort(replacements, orderByAUB);

        return replacements;
    }

    private void manageVacation(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (Status.VACATION.equals(patientMultiSearch.getStatus())) {
            replacements.put("vacation", String.valueOf(patientMultiSearch.getStatus()));
        } else {
            if (Status.ARCHIVE.equals(patientMultiSearch.getStatus())) {
                replacements.put("archive", String.valueOf(patientMultiSearch.getStatus()));
            }

            if (patientMultiSearch.getSectorId() != null) {
                replacements.put(PARAM_SECTOR, String.valueOf(patientMultiSearch.getSectorId()));
            }
        }
    }

    private void manageUnit(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getUnitId() != null && patientMultiSearch.getUnitId() != 0) {
            replacements.put(PARAM_UNIT, String.valueOf(patientMultiSearch.getUnitId()));
        }
    }

    private void manageDoctor(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getDoctorId() != null && patientMultiSearch.getDoctorId() != 0) {
            replacements.put(PARAM_DOCTOR, String.valueOf(patientMultiSearch.getDoctorId()));
        }
    }

    private void manageRecordNumber(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getRecordNumber() != null) {
            replacements.put(PARAM_RECORD_NUMBER, String.valueOf(patientMultiSearch.getRecordNumber()));
        }
    }

    private void manageLastname(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (StringUtils.isNotEmpty(patientMultiSearch.getName())) {
            replacements.put(PARAM_NAME, String.valueOf(patientMultiSearch.getName()));
        }
    }

    private void manageFirstname(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (StringUtils.isNotEmpty(patientMultiSearch.getFirstName())) {
            replacements.put(PARAM_FIRSTNAME, String.valueOf(patientMultiSearch.getFirstName()));
        }
    }

    private void manageBirthday(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getBirthDay() != null) {
            replacements.put(PARAM_BIRTHDAY, String.valueOf(patientMultiSearch.getBirthDay()));
        }
    }

    private void manageGender(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getGender() != null && patientMultiSearch.getGender() != -1) {
            replacements.put(PARAM_GENDER, String.valueOf(patientMultiSearch.getGender()));
        }
    }

    private void manageAttendance(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (patientMultiSearch.getAttendance() != null && !Attendance.ALL.equals(patientMultiSearch.getAttendance())) {
            replacements.put(PARAM_ATTENDANCE, String.valueOf(patientMultiSearch.getAttendance()));
        }
    }

    private void manageStatus(final Map<String, String> replacements, final PatientSearch patientMultiSearch) {
        if (Health.NORMAL.equals(patientMultiSearch.getHealth())) {
            replacements.put("statusNormal", Boolean.TRUE.toString());
        } else if (Health.GOOD.equals(patientMultiSearch.getHealth())) {
            replacements.put("statusGood", Boolean.TRUE.toString());
        } else if (patientMultiSearch.getStatus() != null && !Distance.UNKNOWN.equals(patientMultiSearch.getStatus())) {
            replacements.put(PARAM_STATUS, patientMultiSearch.getStatus().name());
        }
    }

    private void manageSort(final Map<String, String> replacements, final Map<String, Boolean> orderBy) {
        if (orderBy != null && orderBy.size() > 0) {

            for (Entry<String, Boolean> order : orderBy.entrySet()) {
                if (PATTERN_COLUMN.matcher(order.getKey()).matches()) {
                    replacements.put("orderBy", order.getKey() + " " + this.getDirection(order.getValue()));
                }
            }
        }
    }

    private String getDirection(final boolean direction) {
        if (direction) {
            return "ASC";
        }
        return "DESC";
    }
}