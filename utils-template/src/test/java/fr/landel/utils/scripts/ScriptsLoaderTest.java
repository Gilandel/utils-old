/*
 * #%L
 * utils-scripts
 * %%
 * Copyright (C) 2016 - 2017 Gilandel
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
import static org.junit.Assert.assertNull;
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

import fr.landel.utils.assertor.Assertor;
import fr.landel.utils.commons.StringUtils;
import fr.landel.utils.io.FileUtils;
import fr.landel.utils.scripts.PatientSearch.Attendance;
import fr.landel.utils.scripts.PatientSearch.Distance;
import fr.landel.utils.scripts.PatientSearch.Health;
import fr.landel.utils.scripts.PatientSearch.Status;

/**
 * Check scripts loader
 *
 * @since Dec 1, 2015
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

    private static final String PATH = "src/test/resources/my_scripts/";

    private ScriptsLoader scriptsLoader;

    /**
     * initialize the loader with scripts list
     * 
     * @throws IOException
     *             On failures
     */
    @Before
    public void init() throws IOException {
        this.scriptsLoader = new ScriptsLoader();

        this.scriptsLoader.setPath("my_scripts");
        this.scriptsLoader.init(EnumScripts.values());

        assertNotNull(this.scriptsLoader.getReplacer());
    }

    /**
     * Test single script loader
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testSingleScript() throws IOException {
        final ScriptsLoader loader = new ScriptsLoader("my_scripts");
        final ScriptsList<?> script = loader.init("test.sql", StandardCharsets.UTF_8);
        final StringBuilder builder = loader.get(script, "app.id", "my_best_app");

        assertEquals("select * from test where id = 'my_best_app'", builder.toString());

        assertEquals(1, script.getValues().length);
    }

    /**
     * Test scripts loader
     * 
     * @throws IOException
     *             On error
     */
    @Test
    public void testGetScripts() throws IOException {

        // STANDARD TEMPLATE
        StringBuilder content = this.scriptsLoader.get(EnumScripts.TEST);
        assertNotNull(content);
        assertEquals("select * from test where id = ''", content.toString());

        content = this.scriptsLoader.get(EnumScripts.TEST_ONE_LINE, "app.id", "app_id");
        assertNotNull(content);
        assertEquals("select * from test where id = 'app_id' ", content.toString());

        assertNull(this.scriptsLoader.get(null));
        assertNull(this.scriptsLoader.get(null, null));
        assertNull(this.scriptsLoader.get(null, null, null));

        // JSON TEMPLATE
        final ScriptsLoader loader = new ScriptsLoader();

        assertNotNull(loader);

        loader.setPath("my_scripts");
        loader.getReplacer().setTemplate(ScriptsTemplate.TEMPLATE_JSON);

        loader.init(EnumScripts.INDEX_AGGS);

        StringBuilder builder = loader.get(EnumScripts.INDEX_AGGS, "apps", "my_app_id");
        StringBuilder expected = FileUtils.getFileContent(PATH + "index.expected.elastic");

        assertTrue(Assertor.that(builder).isEqualIgnoreLineReturns(expected).isOK());
    }

    /**
     * Test scripts loader
     */
    @Test
    public void test() {
        String key = "app.id";
        String replacement = "28";
        StringBuilder builder = this.scriptsLoader.get(EnumScripts.TEST, key, replacement);
        assertNotNull(builder);
        assertEquals(-1, builder.indexOf(key));
        assertTrue(builder.indexOf(replacement) > -1);
        assertEquals(-1, builder.indexOf("{"));
        assertEquals(-1, builder.indexOf("}"));
    }

    /**
     * Test scripts loader
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

        LOGGER.info("Call scriptsLoader for patientSearch");

        StringBuilder builder = FileUtils.convertToUnix(this.scriptsLoader.get(EnumScripts.PATIENTS_SEARCH, replacements));

        LOGGER.info("scriptsLoader for patientSearch done");

        assertNotNull(builder);
        try {
            StringBuilder expected = FileUtils
                    .convertToUnix(FileUtils.getFileContent(PATH + "patientsSearch.expected.sql", StandardCharsets.UTF_8));

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
