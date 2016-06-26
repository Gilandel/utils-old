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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang3.tuple.MutablePair;
import org.apache.commons.lang3.tuple.Pair;

import fr.landel.utils.asserts.AssertUtils;
import fr.landel.utils.commons.StringUtils;

/**
 * Scripts replacer (parse, execute and replace all scripts). Default
 * expressions and operator are configured for scripts like SQL.
 *
 * @since 1 déc. 2015
 * @author Gilles
 *
 */
public class ScriptsReplacer {

    /**
     * The open expression tag (use for expressions and variables)
     */
    public static final String EXPRESSION_OPEN = "{";

    /**
     * The close expression tag (use for expressions and variables)
     */
    public static final String EXPRESSION_CLOSE = "}";

    /**
     * The open block in expression
     * 
     * <pre>
     * ex: {(a &amp;&amp; b) || (b &amp;&amp; c) ?? ...}
     * </pre>
     */
    public static final String BLOCK_OPEN = "(";

    /**
     * The close block in expression
     * 
     * <pre>
     * ex: {(a &amp;&amp; b) || (b &amp;&amp; c) ?? ...}
     * </pre>
     */
    public static final String BLOCK_CLOSE = ")";

    /**
     * The THEN operator
     */
    public static final String OPERATOR_THEN = "??";

    /**
     * The ELSE operator
     */
    public static final String OPERATOR_ELSE = "::";

    /**
     * The AND operator
     */
    public static final String OPERATOR_AND = "&&";

    /**
     * The OR operator
     */
    public static final String OPERATOR_OR = "||";

    /**
     * The NOT operator
     */
    public static final String OPERATOR_NOT = "!";

    private static final String SINGLE_QUOTE = "'";

    private String expressionOpen;
    private String expressionClose;
    private String blockOpen;
    private String blockClose;
    private String operatorThen;
    private String operatorElse;
    private String operatorAnd;
    private String operatorOr;
    private String operatorNot;

    /**
     * Constructor
     */
    public ScriptsReplacer() {
        super();

        this.expressionOpen = EXPRESSION_OPEN;
        this.expressionClose = EXPRESSION_CLOSE;
        this.blockOpen = BLOCK_OPEN;
        this.blockClose = BLOCK_CLOSE;
        this.operatorThen = OPERATOR_THEN;
        this.operatorElse = OPERATOR_ELSE;
        this.operatorAnd = OPERATOR_AND;
        this.operatorOr = OPERATOR_OR;
        this.operatorNot = OPERATOR_NOT;
    }

    /**
     * @param expressionOpen
     *            the open expression string (default: "{")
     */
    public void setExpressionOpen(String expressionOpen) {
        AssertUtils.check(expressionOpen).isNotEmpty();
        this.expressionOpen = expressionOpen;
    }

    /**
     * @param expressionClose
     *            the close expression string (default: "}")
     */
    public void setExpressionClose(String expressionClose) {
        AssertUtils.check(expressionClose).isNotEmpty();
        this.expressionClose = expressionClose;
    }

    /**
     * @param blockOpen
     *            the open block string following the language (default: "(")
     */
    public void setBlockOpen(String blockOpen) {
        AssertUtils.check(blockOpen).isNotEmpty();
        this.blockOpen = blockOpen;
    }

    /**
     * @param blockClose
     *            the close block string following the language (default: ")")
     */
    public void setBlockClose(String blockClose) {
        AssertUtils.check(blockClose).isNotEmpty();
        this.blockClose = blockClose;
    }

    /**
     * @param operatorThen
     *            the THEN operator string (default: "??")
     */
    public void setOperatorThen(String operatorThen) {
        AssertUtils.check(operatorThen).isNotEmpty();
        this.operatorThen = operatorThen;
    }

    /**
     * @param operatorElse
     *            the ELSE operator string (default: "::")
     */
    public void setOperatorElse(String operatorElse) {
        AssertUtils.check(operatorElse).isNotEmpty();
        this.operatorElse = operatorElse;
    }

    /**
     * @param operatorAnd
     *            the AND operator string (default: "&amp;&amp;")
     */
    public void setOperatorAnd(String operatorAnd) {
        AssertUtils.check(operatorAnd).isNotEmpty();
        this.operatorAnd = operatorAnd;
    }

    /**
     * @param operatorOr
     *            the OR operator string (default: "||")
     */
    public void setOperatorOr(String operatorOr) {
        AssertUtils.check(operatorOr).isNotEmpty();
        this.operatorOr = operatorOr;
    }

    /**
     * @param operatorNot
     *            the NOT operator string (default: "!")
     */
    public void setOperatorNot(String operatorNot) {
        AssertUtils.check(operatorNot).isNotEmpty();
        this.operatorNot = operatorNot;
    }

    /**
     * Replace all keys by their values in the input string builder.<br>
     * <br>
     * Supported operators: or=||, and=&amp;&amp;, not=!<br>
     * Parenthesis are supported in condition Sub-conditions can be added in
     * value and default<br>
     * <br>
     * Supported format (with replacements: key1=var.name, value1=data,
     * key2=var2, value2=vx):
     * 
     * <pre>
     * - {var.name} will be replaced by "data" without quotes
     * - {var.name??value} will be replaced by "value" without quotes
     * - {var.name::default} will be replaced by "data" without quotes
     * - {var.name&amp;&amp;var2::default} will be replaced by "datavx" without quotes 
     * - {var2 &amp;&amp; var.name::default} will be replaced by "vxdata" without quotes 
     * - {var2 || !var.name::default} will be replaced by "vx" without quotes
     * - {unknown.var} will be replaced by an empty string
     * - {unknown.var::default} will be replaced by "default" without quotes
     * - {var.name??var='{var.name}'} will be replaced by "var='data'" without quotes
     * - {unknown.var??var='{var.name}'} will be replaced an empty string
     * - {unknown.var??var='{var.name}'::default} will be replaced by "default" without quotes
     * </pre>
     * 
     * @param sb
     *            The string builder
     * @param replacements
     *            the replacements (entry: key=value)
     * @param <V>
     *            The type of values
     * @throws IllegalArgumentException
     *             If number of brackets open and close doesn't match. If
     *             brackets are found in key replacement or in value
     *             replacement. If replacement value hasn't pairs of single
     *             quote (avoid some SQL injections but not all, parameters have
     *             to be checked)
     */
    public <V> void replace(final StringBuilder sb, final Map<String, V> replacements) throws IllegalArgumentException {
        this.replace(sb, replacements, false);
    }

    /**
     * Replace all keys by their values in the input string builder.<br>
     * <br>
     * Supported operators: or=||, and=&amp;&amp;, not=!<br>
     * Parenthesis are supported in condition<br>
     * Sub-conditions can be added in value and default<br>
     * <br>
     * Supported format (with replacements: key1=var.name, value1=data,
     * key2=var2, value2=vx):
     * 
     * <pre>
     * - {var.name} will be replaced by "data" without quotes
     * - {var.name??value} will be replaced by "value" without quotes
     * - {var.name::default} will be replaced by "data" without quotes
     * - {var.name&amp;&amp;var2::default} will be replaced by "datavx" without quotes
     * - {var2 &amp;&amp; var.name::default} will be replaced by "vxdata" without quotes
     * - {var2 || !var.name::default} will be replaced by "vx" without quotes
     * - {unknown.var} will be replaced by an empty string
     * - {unknown.var::default} will be replaced by "default" without quotes
     * - {var.name??var='{var.name}'} will be replaced by "var='data'" without quotes
     * - {unknown.var??var='{var.name}'} will be replaced en empty string
     * - {unknown.var??var='{var.name}'::default} will be replaced by "default" without quotes
     * </pre>
     * 
     * @param sb
     *            The string builder
     * @param replacements
     *            the replacements (entry: key=value)
     * @param checkVariables
     *            En/Disable variables to avoid SQL injections (check single
     *            quote in variables)
     * @param <V>
     *            The type of values
     * @throws IllegalArgumentException
     *             If number of brackets open and close doesn't match. If
     *             brackets are found in key replacement or in value
     *             replacement. If replacement value hasn't pairs of single
     *             quote (avoid some SQL injections but not all, parameters have
     *             to be checked)
     */
    public <V> void replace(final StringBuilder sb, final Map<String, V> replacements, final boolean checkVariables)
            throws IllegalArgumentException {
        // first check if the input is valid
        this.checkInput(sb);

        final Map<String, String> replacementsSTR = new HashMap<>();
        for (Entry<String, V> entry : replacements.entrySet()) {
            replacementsSTR.put(entry.getKey(), String.valueOf(entry.getValue()));
        }

        // check if replacements ar valid
        this.checkReplacements(replacementsSTR, checkVariables);

        // replace all keys with their variables
        for (Entry<String, String> entry : replacementsSTR.entrySet()) {
            this.replaceSimples(sb, entry.getKey(), entry.getValue());
        }

        // clear all unknown variables
        this.clearUnknownSimples(sb);

        // replace conditions
        this.replaceConditions(sb, replacementsSTR);
    }

    private void checkInput(final StringBuilder sb) throws IllegalArgumentException {
        AssertUtils.check(sb).isNotNull("Input cannot be null").isNotEmpty("Input cannot be empty");

        int countBracketOpen = 0;
        int countBracketClose = 0;

        // get first
        int index = sb.indexOf(this.expressionOpen);
        int indexStop;

        for (; index > -1;) {

            indexStop = sb.indexOf(this.expressionClose, index);
            if (indexStop > -1) {
                countBracketClose++;
            }

            // get next
            index = sb.indexOf(this.expressionOpen, index + 1);
            if (index > -1) {
                indexStop = sb.indexOf(this.expressionClose, index);
            }

            countBracketOpen++;
        }

        AssertUtils.check(countBracketOpen).isEqual(countBracketClose,
                "The count of " + this.expressionOpen + " doesn't match the count of " + this.expressionClose + ", input: " + sb);
    }

    private void checkReplacements(final Map<String, String> replacements, final boolean checkVariables) throws IllegalArgumentException {
        for (Entry<String, String> entry : replacements.entrySet()) {
            AssertUtils.check(entry.getKey()).isNotNull("Replacement key cannot be null");
            AssertUtils.check(entry.getValue()).isNotNull("Replacement value cannot be null");

            final String errorKey = "Replacement key cannot contains: ";
            AssertUtils.check(entry.getKey()).doesNotContain(this.expressionOpen, errorKey + this.expressionOpen)
                    .doesNotContain(this.expressionClose, errorKey + this.expressionClose)
                    .doesNotContain(this.operatorThen, errorKey + this.operatorThen)
                    .doesNotContain(this.operatorElse, errorKey + this.operatorElse);

            final String errorValue = "Replacement value cannot contains: ";
            AssertUtils.check(entry.getValue()).doesNotContain(this.expressionOpen, errorValue + this.expressionOpen)
                    .doesNotContain(this.expressionClose, errorValue + this.expressionClose)
                    .doesNotContain(this.operatorThen, errorValue + this.operatorThen)
                    .doesNotContain(this.operatorElse, errorValue + this.operatorElse);

            if (checkVariables) {
                // Avoid some SQL injections but not all!, parameters has to be
                // checked before
                AssertUtils.check(StringUtils.countMatches(entry.getValue(), SINGLE_QUOTE) % 2).isEqual(0,
                        "Replacement value has to contain only pairs of: " + SINGLE_QUOTE);
                AssertUtils.check(
                        StringUtils.countMatches(StringUtils.replace(entry.getValue(), SINGLE_QUOTE + SINGLE_QUOTE, ""), SINGLE_QUOTE))
                        .isEqual(0, "Replacement value has to contain only group of pairs of: " + SINGLE_QUOTE);
            }
        }
    }

    private void replaceSimples(final StringBuilder sb, final String key, final String value) {
        String simple;

        // get first
        int index = sb.indexOf(this.expressionOpen);
        int indexStop = sb.indexOf(this.expressionClose, index);

        for (; index > -1 && indexStop > -1;) {
            simple = sb.substring(index + 1, indexStop);

            if (key.equals(simple.trim())) {
                sb.replace(index, indexStop + 1, value);
            }

            // get next
            index = sb.indexOf(this.expressionOpen, index + 1);
            if (index > -1) {
                indexStop = sb.indexOf(this.expressionClose, index);
            }
        }
    }

    private void clearUnknownSimples(final StringBuilder sb) {
        String simple;
        int indexValid;
        int indexDefault;

        // get first
        int index = sb.indexOf(this.expressionOpen);
        int indexStop = sb.indexOf(this.expressionClose, index);

        for (; index > -1 && indexStop > -1;) {
            simple = sb.substring(index + 1, indexStop);

            indexValid = simple.indexOf(this.operatorThen);
            indexDefault = simple.indexOf(this.operatorElse);

            if (indexValid == -1 && indexDefault == -1) {
                sb.replace(index, indexStop + 1, "");
            }

            // get next
            index = sb.indexOf(this.expressionOpen, index + 1);
            if (index > -1) {
                indexStop = sb.indexOf(this.expressionClose, index);
            }
        }
    }

    private void replaceConditions(final StringBuilder sb, final Map<String, String> replacements) {
        Pair<Integer, Integer> bounds;
        String condition;
        int indexValid;
        int indexDefault;
        String expression;
        String value;
        String defaultValue;

        for (bounds = this.findDeepestCondition(sb, this.expressionOpen, this.expressionClose); bounds != null; bounds = this
                .findDeepestCondition(sb, this.expressionOpen, this.expressionClose)) {

            condition = sb.substring(bounds.getLeft(), bounds.getRight());

            indexValid = condition.indexOf(this.operatorThen);
            indexDefault = condition.indexOf(this.operatorElse, indexValid);

            expression = null;
            value = null;
            defaultValue = null;
            if (indexValid > -1) {
                expression = condition.substring(0, indexValid);
                if (indexDefault > -1) {
                    value = condition.substring(indexValid + this.operatorThen.length(), indexDefault);
                    defaultValue = condition.substring(indexDefault + this.operatorElse.length());
                } else {
                    value = condition.substring(indexValid + this.operatorThen.length());
                }
            } else if (indexDefault > -1) {
                expression = condition.substring(0, indexDefault);
                defaultValue = condition.substring(indexDefault + this.operatorElse.length());
            }

            if (expression != null) {
                sb.replace(bounds.getLeft() - 1, bounds.getRight() + 1,
                        this.replaceCondition(replacements, expression, value, defaultValue));
            } else {
                sb.replace(bounds.getLeft() - 1, bounds.getRight() + 1, "");
            }
        }
    }

    private Pair<Integer, Integer> findDeepestCondition(final StringBuilder sb, final String inStr, final String outStr) {
        Pair<Integer, Integer> bounds = null;

        // get first
        int index = sb.indexOf(inStr);
        int indexStop = sb.indexOf(outStr, index + inStr.length());

        if (index > -1 && indexStop > -1) {
            for (; index > -1 && index < indexStop;) {
                bounds = new MutablePair<Integer, Integer>(index + inStr.length(), indexStop);

                // get next
                index = sb.indexOf(inStr, index + inStr.length());
            }
        }

        return bounds;
    }

    private String replaceCondition(final Map<String, String> replacements, final String expression, final String value,
            final String defaultValue) {

        final List<String> varChecker = new ArrayList<>();
        final Set<String> keys = replacements.keySet();

        StringBuilder result = new StringBuilder();

        if (this.checkExpression(expression, keys, varChecker)) {
            if (value != null) {
                result.append(value);
            } else {
                for (String variable : varChecker) {
                    if (variable.indexOf(this.operatorNot) == -1) {
                        result.append(replacements.get(variable));
                    }
                }
            }
        } else if (defaultValue != null) {
            result.append(defaultValue);
        }

        return result.toString();
    }

    private boolean checkExpression(final String expression, final Set<String> keys, final List<String> variables) {
        Pair<Integer, Integer> bounds;
        StringBuilder sb = new StringBuilder(expression);
        String condition;
        boolean isOk = false;

        for (bounds = this.findDeepestCondition(sb, this.blockOpen, this.blockClose); bounds != null; bounds = this.findDeepestCondition(sb,
                this.blockOpen, this.blockClose)) {

            condition = sb.substring(bounds.getLeft(), bounds.getRight());

            isOk = this.checkVariables(keys, variables, condition, isOk);

            sb.replace(bounds.getLeft() - 1, bounds.getRight() + 1, Boolean.toString(isOk));
        }

        return this.checkVariables(keys, variables, sb.toString(), isOk);
    }

    private boolean checkVariables(final Set<String> keys, final List<String> variables, final String condition, final boolean isOk) {
        boolean isOkVar = isOk;
        boolean previousOr = true;
        boolean currentOr = false;
        int previousIndex = 0;
        String variable = null;
        int indexAnd;
        int indexOr;

        indexAnd = condition.indexOf(this.operatorAnd);
        indexOr = condition.indexOf(this.operatorOr);
        if (indexAnd > -1 || indexOr > -1) {
            for (; indexAnd > -1 || indexOr > -1;) {

                if (this.checkOperators(indexAnd, indexOr)) {
                    variable = condition.substring(previousIndex, indexAnd).trim();
                    previousIndex = indexAnd + this.operatorAnd.length();
                    currentOr = false;
                } else if (this.checkOperators(indexOr, indexAnd)) {
                    variable = condition.substring(previousIndex, indexOr).trim();
                    previousIndex = indexOr + this.operatorOr.length();
                    currentOr = true;
                }

                isOkVar = this.add(keys, variable, isOkVar, previousOr, variables);

                indexAnd = condition.indexOf(this.operatorAnd, previousIndex);
                indexOr = condition.indexOf(this.operatorOr, previousIndex);
                previousOr = currentOr;
            }
            isOkVar = this.add(keys, condition.substring(previousIndex).trim(), isOkVar, previousOr, variables);
        } else {
            isOkVar = this.add(keys, condition.trim(), isOkVar, previousOr, variables);
        }

        return isOkVar;
    }

    private boolean checkOperators(final int indexOperator1, final int indexOperator2) {
        return indexOperator1 > -1 && (indexOperator2 == -1 || indexOperator1 < indexOperator2);
    }

    private boolean add(final Set<String> keys, final String variable, final boolean isOk, final boolean previousOr,
            final List<String> variables) {
        boolean is = false;

        if (variable != null) {
            if (Boolean.TRUE.toString().equals(variable)) {
                is = true;
            } else if (!Boolean.FALSE.toString().equals(variable)) {
                boolean isNot = variable.indexOf(this.operatorNot) == 0;
                if (!isNot && keys.contains(variable)) {
                    variables.add(variable);
                    is = true;
                } else if (isNot) {
                    is = !keys.contains(variable.substring(this.operatorNot.length()).trim());
                }
            }
        }

        if (previousOr) {
            return isOk || is;
        }
        return isOk && is;
    }
}
