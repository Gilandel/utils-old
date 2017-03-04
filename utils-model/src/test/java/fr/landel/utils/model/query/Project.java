/*
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
package fr.landel.utils.model.query;

import java.util.Date;
import java.util.Map;

import javax.persistence.Transient;

import fr.landel.utils.commons.DateUtils;
import fr.landel.utils.model.AbstractEntity;

/**
 * The project DTO for tests.
 *
 * @since Nov 30, 2015
 * @author Gilles
 *
 */
public class Project extends AbstractEntity<Project, Integer> {

    /**
     * Serial
     */
    private static final long serialVersionUID = -8001724235265011351L;

    /**
     * Id
     */
    private Integer id;

    /**
     * Zone
     */
    private Zone zone;

    /**
     * PtDepart
     */
    private String ptDepart;

    /**
     * PtArrivee
     */
    private String ptArrivee;

    /**
     * Label
     */
    private String label;

    /**
     * Date (and time) of project creation
     */
    private Date dateCreation;

    /**
     * Urgent
     */
    private Boolean urgent;

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
     * Constructor
     */
    public Project() {
        super(Project.class);
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
     * @return the zone
     */
    public Zone getZone() {
        return this.zone;
    }

    /**
     * @param zone
     *            the zone to set
     */
    public void setZone(Zone zone) {
        this.zone = zone;
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
     * @return creation date of the project
     */
    public Date getDateCreation() {
        return DateUtils.cloneDate(this.dateCreation);
    }

    /**
     * @param dateCreation
     *            creation date of the project
     */
    public void setDateCreation(Date dateCreation) {
        this.dateCreation = DateUtils.cloneDate(dateCreation);
    }

    /**
     * @return the urgent
     */
    public Boolean getUrgent() {
        return this.urgent;
    }

    /**
     * @param urgent
     *            the urgent to set
     */
    public void setUrgent(Boolean urgent) {
        this.urgent = urgent;
    }

    /**
     * @return the ptDepart
     */
    public String getPtDepart() {
        return this.ptDepart;
    }

    /**
     * @param ptDepart
     *            the ptDepart to set
     */
    public void setPtDepart(String ptDepart) {
        this.ptDepart = ptDepart;
    }

    /**
     * @return the ptArrivee
     */
    public String getPtArrivee() {
        return this.ptArrivee;
    }

    /**
     * @param ptArrivee
     *            the ptArrivee to set
     */
    public void setPtArrivee(String ptArrivee) {
        this.ptArrivee = ptArrivee;
    }

    /**
     * @return the name
     */
    @Transient
    public String getName() {
        StringBuilder name = new StringBuilder();
        if (this.ptDepart != null) {
            name.append(this.ptDepart);
        }
        if (this.ptDepart != null && this.ptArrivee != null) {
            name.append(" - ");
        }
        if (this.ptArrivee != null) {
            name.append(this.ptArrivee);
        }
        return name.toString();
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
    protected int overCompareTo(Project obj) {
        return this.id.compareTo(obj.id);
    }

    @Override
    protected boolean overEquals(Project obj) {
        return this.id.equals(obj.id);
    }

    @Override
    protected int overHashCode() {
        return this.id.hashCode();
    }
}
