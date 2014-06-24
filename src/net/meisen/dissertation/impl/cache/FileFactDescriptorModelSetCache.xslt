<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:file="http://dev.meisen.net/xsd/dissertation/caches/facts/file">

  <xsl:template match="file:config">
    <bean class="net.meisen.dissertation.impl.cache.FileBitmapIdCacheConfig">
      <xsl:if test="@folder">
        <xsl:variable name="folder" select="@folder" />
        <property name="location" value="{$folder}" />
      </xsl:if>

      <xsl:if test="@size">
        <xsl:variable name="size" select="@size" />
        <property name="cacheSize" value="{$size}" />
      </xsl:if>
      
      <xsl:if test="@maxFileSize">
        <xsl:variable name="maxFileSize" select="@maxFileSize" />
        <property name="maxFileSize" value="{$maxFileSize}" />
      </xsl:if>
      
      <xsl:if test="@cleaningFactor">
        <xsl:variable name="cleaningFactor" select="@cleaningFactor" />
        <property name="cacheCleaningFactor" value="{$cleaningFactor}" />
      </xsl:if>
    </bean>
  </xsl:template>
</xsl:stylesheet>