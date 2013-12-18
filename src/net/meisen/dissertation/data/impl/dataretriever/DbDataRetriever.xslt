<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:db="http://dev.meisen.net/xsd/dissertation/model/db">

  <xsl:template match="db:connection">
    <bean class="net.meisen.dissertation.data.impl.dataretriever.DbConnectionConfig">
      <xsl:variable name="type" select="@type" />
      <xsl:variable name="url" select="@url" />
      <xsl:variable name="driver" select="@driver" />
      <xsl:variable name="username" select="@username" />
      <xsl:variable name="password" select="@password" />
    
      <property name="type" value="{$type}"/>
      <property name="url" value="{$url}"/>
      <property name="driver" value="{$driver}"/>
      <property name="username" value="{$username}"/>
      <property name="password" value="{password}"/>
    </bean>
  </xsl:template> 
</xsl:stylesheet>