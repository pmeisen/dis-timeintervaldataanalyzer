<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:file="http://dev.meisen.net/xsd/dissertation/cache/file">

  <xsl:template match="file:location">
    <bean class="net.meisen.dissertation.impl.cache.FileCacheConfig">
      <xsl:variable name="folder" select="@folder" />
      <constructor-arg type="java.lang.String" value="{$folder}" />
    </bean>
  </xsl:template>
</xsl:stylesheet>