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
package fr.landel.utils.model.query;

import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.OneToMany;

import fr.landel.utils.commons.DateUtils;
import fr.landel.utils.model.AbstractEntity;

/**
 * The zone DTO for query tests.
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
public class Zone extends AbstractEntity<Zone, Integer> {

    /**
     * Serial
     */
    private static final long serialVersionUID = -7897422323278360063L;

    /**
     * Id
     */
    private Integer id;

    /**
     * Code insee
     */
    private String codeInsee;

    /**
     * Label
     */
    private String label;

    /**
     * Estimated end date
     */
    private Date estimatedEndDate;

    /**
     * Latitude min
     */
    private Double latitudeMin;

    /**
     * Latitude max
     */
    private Double latitudeMax;

    /**
     * Longitude min
     */
    private Double longitudeMin;

    /**
     * Longitude max
     */
    private Double longitudeMax;

    /**
     * Latitude centroid
     */
    private Double latitudeCentroid;

    /**
     * Longitude centroid
     */
    private Double longitudeCentroid;

    /**
     * Projects
     */
    @OneToMany(mappedBy = "zone", cascade = CascadeType.ALL, orphanRemoval = true)
    private Set<Project> projects;

    /**
     * Constructor
     */
    public Zone() {
        super(Zone.class);

        this.projects = new HashSet<>();
    }

    /**
     * @return the id
     */
    public Integer getId() {
        return this.id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(Integer id) {
        this.id = id;
    }

    /**
     * @return the codeInsee
     */
    public String getCodeInsee() {
        return this.codeInsee;
    }

    /**
     * @param codeInsee
     *            the codeInsee to set
     */
    public void setCodeInsee(String codeInsee) {
        this.codeInsee = codeInsee;
    }

    /**
     * @return the label
     */
    public String getLabel() {
        return this.label;
    }

    /**
     * @param label
     *            the label to set
     */
    public void setLabel(String label) {
        this.label = label;
    }

    /**
     * @return the estimatedEndDate
     */
    public Date getEstimatedEndDate() {
        return DateUtils.getDate(this.estimatedEndDate);
    }

    /**
     * @param estimatedEndDate
     *            the estimatedEndDate to set
     */
    public void setEstimatedEndDate(Date estimatedEndDate) {
        this.estimatedEndDate = DateUtils.getDate(estimatedEndDate);
    }

    /**
     * @return the projects
     */
    public Set<Project> getProjects() {
        return this.projects;
    }

    /**
     * @param projects
     *            the projects to set
     */
    public void setProjects(Set<Project> projects) {
        this.projects = projects;
    }

    /**
     * @return the min latitude minimum latitude
     */
    public Double getLatitudeMin() {
        return this.latitudeMin;
    }

    /**
     * @param latitudeMin
     *            latitudeMin to set
     */
    public void setLatitudeMin(Double latitudeMin) {
        this.latitudeMin = latitudeMin;
    }

    /**
     * @return the max latitude maximum latitude
     */
    public Double getLatitudeMax() {
        return this.latitudeMax;
    }

    /**
     * @param latitudeMax
     *            latitudeMax to set
     */
    public void setLatitudeMax(Double latitudeMax) {
        this.latitudeMax = latitudeMax;
    }

    /**
     * @return the min longitude minimum longitude
     */
    public Double getLongitudeMin() {
        return this.longitudeMin;
    }

    /**
     * @param longitudeMin
     *            longitudeMin to set
     */
    public void setLongitudeMin(Double longitudeMin) {
        this.longitudeMin = longitudeMin;
    }

    /**
     * @return the max longitude maximum longitude
     */
    public Double getLongitudeMax() {
        return this.longitudeMax;
    }

    /**
     * @param longitudeMax
     *            longitudeMax to set
     */
    public void setLongitudeMax(Double longitudeMax) {
        this.longitudeMax = longitudeMax;
    }

    /**
     * @return the latitudeCentroid
     */
    public Double getLatitudeCentroid() {
        return this.latitudeCentroid;
    }

    /**
     * @param latitudeCentroid
     *            the latitudeCentroid to set
     */
    public void setLatitudeCentroid(Double latitudeCentroid) {
        this.latitudeCentroid = latitudeCentroid;
    }

    /**
     * @return the longitudeCentroid
     */
    public Double getLongitudeCentroid() {
        return this.longitudeCentroid;
    }

    /**
     * @param longitudeCentroid
     *            the longitudeCentroid to set
     */
    public void setLongitudeCentroid(Double longitudeCentroid) {
        this.longitudeCentroid = longitudeCentroid;
    }

    @Override
    public Integer getPrimaryKey() {
        return this.getId();
    }

    @Override
    public void setPrimaryKey(Integer key) {
        this.setId(key);
    }

    @Override
    protected void overToString(Map<String, Object> map) {
        map.put("id", this.id);
        map.put("label", this.label);
    }

    @Override
    protected int overCompareTo(Zone obj) {
        return this.id.compareTo(obj.id);
    }

    @Override
    protected boolean overEquals(Zone obj) {
        return this.id.equals(obj.id);
    }

    @Override
    protected int overHashCode() {
        return this.id.hashCode();
    }
}
