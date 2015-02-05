<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:csv="http://dev.meisen.net/xsd/dissertation/model/csv">

  <xsl:template match="csv:config">
    <bean class="net.meisen.dissertation.impl.dataretriever.CsvDataConfig">
      <xsl:if test="@classpath">
      	<property name="classpath"><value><xsl:value-of select="@classpath" /></value></property>
      </xsl:if>
      <xsl:if test="@separator">
      	<property name="separator"><value><xsl:value-of select="@separator" /></value></property>
      </xsl:if>
      
      <property name="file"><value><xsl:value-of select="@file" /></value></property>
    </bean>
  </xsl:template>
  
  <xsl:template match="csv:selector">
    <bean class="net.meisen.dissertation.impl.dataretriever.CsvDataSelector">
      <xsl:if test="@position">
      	<property name="position"><value><xsl:value-of select="@position" /></value></property>
      </xsl:if>
      <xsl:if test="@column">
      	<property name="column"><value><xsl:value-of select="@column" /></value></property>      	
      </xsl:if>
    </bean>
  </xsl:template>
</xsl:stylesheet>