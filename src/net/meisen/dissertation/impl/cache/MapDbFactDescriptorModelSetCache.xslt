<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:mapdb="http://dev.meisen.net/xsd/dissertation/caches/facts/mapdb">

  <xsl:template match="mapdb:config">
    <bean class="net.meisen.dissertation.impl.cache.MapDbBitmapIdCacheConfig">
      <xsl:if test="@folder">
        <xsl:variable name="folder" select="@folder" />
        <property name="location" value="{$folder}" />
      </xsl:if>

      <xsl:if test="@size">
        <xsl:variable name="size" select="@size" />
        <property name="cacheSize" value="{$size}" />
      </xsl:if>
      
      <xsl:if test="@type">
        <xsl:variable name="type" select="@type" />
        <property name="mapDbType" value="{$type}" />
      </xsl:if>
     
    </bean>
  </xsl:template>
</xsl:stylesheet>