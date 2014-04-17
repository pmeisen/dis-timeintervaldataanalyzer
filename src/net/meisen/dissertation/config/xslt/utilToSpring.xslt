<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0"
        xmlns="http://www.springframework.org/schema/beans"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="xml" indent="yes" />
  
  <xsl:template name="csvToValues">
    <xsl:param name="list" />
    <xsl:param name="itemSeparator" />
    
    <xsl:variable name="value" select="normalize-space(substring-before(concat($list, $itemSeparator), $itemSeparator))" />

    <xsl:if test="$value">   
      <value><xsl:value-of select="$value" /></value>

      <xsl:call-template name="csvToValues">
        <xsl:with-param name="list" select="substring-after($list, $itemSeparator)" />
        <xsl:with-param name="itemSeparator" select="$itemSeparator" />
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
  
  <xsl:template name="csvToBeans">
    <xsl:param name="list" />
    <xsl:param name="itemSeparator" />
    
    <xsl:variable name="class" select="normalize-space(substring-before(concat($list, $itemSeparator), $itemSeparator))" />
       
    <xsl:if test="$class">   
      <bean class="{$class}" />

      <xsl:call-template name="csvToBeans">
        <xsl:with-param name="list" select="substring-after($list, $itemSeparator)" />
        <xsl:with-param name="itemSeparator" select="$itemSeparator" />
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
    
  <xsl:template name="mapToEntries">
    <xsl:param name="list" />
    <xsl:param name="itemSeparator" />
    <xsl:param name="keyValueSeparator" />
    
    <xsl:variable name="item" select="normalize-space(substring-before(concat($list, $itemSeparator), $itemSeparator))" />
       
    <xsl:if test="$item">
      <xsl:variable name="key" select="substring-before($item, $keyValueSeparator)" />
      <xsl:variable name="value" select="substring-after($item, $keyValueSeparator)" />
    
      <entry key="{$key}" value="{$value}" />

      <xsl:call-template name="mapToEntries">
        <xsl:with-param name="list" select="substring-after($list, $itemSeparator)" />
        <xsl:with-param name="itemSeparator" select="$itemSeparator" />
        <xsl:with-param name="keyValueSeparator" select="$keyValueSeparator" />
      </xsl:call-template>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>