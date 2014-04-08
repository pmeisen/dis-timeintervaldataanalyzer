<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:idx="http://dev.meisen.net/xsd/dissertation/model/indexes">

  <xsl:template match="idx:config">
    <bean class="net.meisen.dissertation.impl.indexes.IndexFactoryConfig">
        
      <xsl:if test="@byte"><xsl:variable name="byteClass" select="@byte" /><property name="byteClass" value="{$byteClass}"/></xsl:if>
      <xsl:if test="@short"><xsl:variable name="shortClass" select="@short" /><property name="shortClass" value="{$shortClass}"/></xsl:if>
      <xsl:if test="@int"><xsl:variable name="intClass" select="@int" /><property name="intClass" value="{$intClass}"/></xsl:if>
      <xsl:if test="@long"><xsl:variable name="longClass" select="@long" /><property name="longClass" value="{$longClass}"/></xsl:if>
      
      <xsl:if test="@bitmap"><xsl:variable name="bitmapClass" select="@bitmap" /><property name="bitmapClass" value="{$bitmapClass}"/></xsl:if>
    </bean>
  </xsl:template>
</xsl:stylesheet>