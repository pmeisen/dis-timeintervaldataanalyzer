<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:spp="http://dev.meisen.net/xsd/dissertation/preprocessor/script">

  <xsl:template match="spp:script">
    <bean class="net.meisen.dissertation.impl.dataintegration.ScriptPreProcessorConfig">
      <property name="language"><value><xsl:value-of select="@language" /></value></property>     
      <property name="script"><value><xsl:value-of select="normalize-space(.)" /></value></property>
    </bean>
  </xsl:template>
</xsl:stylesheet>