<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:dbdef="net.meisen.dissertation.impl.dataretriever.DbDefaultValues"
                              xmlns:db="http://dev.meisen.net/xsd/dissertation/model/db">

  <xsl:template match="db:connection">
    <bean class="net.meisen.dissertation.impl.dataretriever.DbConnectionConfig">
      <xsl:variable name="type" select="@type" />
      <xsl:variable name="url" select="@url" />
      <xsl:variable name="driver" select="@driver" />
      <xsl:variable name="username" select="@username" />
      <xsl:variable name="password" select="@password" />
    
      <property name="type" value="{$type}"/>
      <property name="url" value="{$url}"/>
      <property name="driver" value="{$driver}"/>
      <property name="username" value="{$username}"/>
      <property name="password" value="{$password}"/>
    </bean>
  </xsl:template>
  
  <xsl:template match="db:query">
    <bean class="net.meisen.dissertation.impl.dataretriever.DbQueryConfig">
      <xsl:variable name="language">
        <xsl:choose>
          <xsl:when test="@language"><xsl:value-of select="@language" /></xsl:when>
          <xsl:otherwise><xsl:value-of select="dbdef:getDefaultLanguage()" /></xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <property name="query"><value><xsl:value-of select="normalize-space(.)" /></value></property>
      <property name="language" value="{$language}"/>
    </bean>
  </xsl:template>
</xsl:stylesheet>