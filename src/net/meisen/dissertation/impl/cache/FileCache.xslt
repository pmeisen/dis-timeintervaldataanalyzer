<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:file="http://dev.meisen.net/xsd/dissertation/cache/file">

  <xsl:template match="file:config">
    <bean class="net.meisen.dissertation.impl.cache.FileCacheConfig">
      <xsl:if test="@folder">
        <xsl:variable name="folder" select="@folder" />
        <property name="location" value="{$folder}" />
      </xsl:if>

      <xsl:if test="file:strategy[@timeThresholdFactor]">
        <xsl:variable name="timeThresholdFactor" select="file:strategy/@timeThresholdFactor" />
        <property name="timeThresholdFactor" value="{$timeThresholdFactor}" />
      </xsl:if>
      
      <xsl:if test="file:strategy[@weightingTime]">
        <xsl:variable name="weightingTime" select="file:strategy/@weightingTime" />
        <property name="weightingTime" value="{$weightingTime}" />
      </xsl:if>
    </bean>
  </xsl:template>
</xsl:stylesheet>