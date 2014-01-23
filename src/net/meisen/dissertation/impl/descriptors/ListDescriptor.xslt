<?xml version="1.0" encoding="UTF-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                              xmlns="http://www.springframework.org/schema/beans"
                              xmlns:advDes="http://dev.meisen.net/xsd/dissertation/model/advancedDescriptors">

  <xsl:template match="advDes:list">
    <xsl:call-template name="beanDescriptorModel">
      <xsl:with-param name="class">net.meisen.dissertation.impl.descriptors.ListDescriptor</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
</xsl:stylesheet>