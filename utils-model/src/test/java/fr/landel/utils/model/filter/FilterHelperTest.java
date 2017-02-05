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
package fr.landel.utils.model.filter;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.Arrays;

import org.junit.Ignore;

import fr.landel.utils.model.mappable.EntityChild;
import fr.landel.utils.model.mappable.EntityParent;
import fr.landel.utils.model.query.QueryBuilder1;

/**
 * Check {@link FilterHelper}
 *
 * @since Aug 15, 2016
 * @author Gilles
 *
 */
// @RunWith(SpringJUnit4ClassRunner.class)
// @ContextConfiguration(loader = AnnotationConfigContextLoader.class, classes =
// TestConfig.class)
// @TestExecutionListeners({DependencyInjectionTestExecutionListener.class})
public class FilterHelperTest { // extends AbstractJUnit4SpringContextTests {

    // @Autowired
    private FilterHelper filterHelper;

    /**
     * Test method for {@link FilterHelper#FilterHelper()}.
     */
    // @Before
    public void testFilterHelper() {
        assertNotNull(this.filterHelper);
    }

    /**
     * Test method for {@link FilterHelper#buildFilters(java.util.List)}.
     */
    @Ignore
    public void testBuildFilters() {
        FilterInfo<EntityParent, String> filter1 = new FilterInfo<>(EntityParent.class, "column1", "value", false, FilterInfo.OP_EQ,
                FilterInfo.TYPE_STRING, "param1");

        FilterInfo<EntityParent, String> filter2 = new FilterInfo<>(EntityParent.class, "column2", true, false, FilterInfo.OP_EXISTS,
                FilterInfo.TYPE_BOOLEAN, "param2");

        FilterInfo<EntityParent, String> filter3 = new FilterInfo<>(EntityParent.class, "column3", 3, false, FilterInfo.OP_LIKE,
                FilterInfo.TYPE_INTEGER, "param3");

        FilterInfo<EntityParent, String> filter4 = new FilterInfo<>(EntityParent.class, "column4", 5L, false, FilterInfo.OP_IN_LIST,
                FilterInfo.TYPE_LONG, "param4");

        QueryBuilder1<EntityParent, String> subQuery = new QueryBuilder1<>(EntityParent.class, "p").select("1").from().where()
                .in("p.id", "param1").and().between("p.value", "param2", "param3").getBuilder();

        System.out.println(new QueryBuilder1<>(EntityChild.class, "c").from().where().exists(subQuery).toString());

        System.out.println(this.filterHelper.buildFilters(Arrays.asList(filter1, filter2, filter3, filter4)));
    }

    /**
     * Test method for
     * {@link FilterHelper#buildFilterParameters(javax.persistence.Query, java.util.List)}.
     */
    @Ignore
    public void testBuildFilterParameters() {
        fail("Not yet implemented");
    }
}
