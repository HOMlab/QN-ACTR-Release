<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" version="1.0" encoding="ISO-8859-1" indent="yes"/>
	<xsl:variable name="solutions" select="solutions"/>
	<xsl:variable name="mvaModel" select="document(string(solutions/@modelDefinitionPath))"/>
	<xsl:template match="/">
		<xsl:processing-instruction name="xml-stylesheet">"type="text/xsl" href="NepTranslatorMVAtoSIM.xslt"</xsl:processing-instruction>
		<model>
			<xsl:attribute name="xsi:noNamespaceSchemaLocation" namespace="http://www.w3.org/2001/XMLSchema-instance">JMTmodel.xsd</xsl:attribute>
			<!-- OLD
			<xsl:copy-of select="$mvaModel/model/parameters"/>
			-->
			<!-- NEW @author Stefano Omini: copies also blocking region elements -->
			<xsl:copy-of select="$mvaModel/model/parameters"/>
			<xsl:copy-of select="$mvaModel/model/blockingRegion"/>
			<!-- end NEW-->
			<solutions ok="true" solutionMethod="{$solutions/@solutionMethod}">
				<xsl:apply-templates select="$mvaModel/model/parameters/stations/*" mode="printSolutions"/>
				<xsl:apply-templates select="$mvaModel/model/blockingRegion" mode="printSolutions_blocking"/>
			</solutions>
		</model>
	</xsl:template>
	
	<xsl:template match="stations/*" mode="printSolutions">
		<stationresults station="{@name}">
			<xsl:apply-templates select="../../classes/*" mode="printSolutions">
				<xsl:with-param name="station">
					<xsl:value-of select="@name"/>
				</xsl:with-param>
			</xsl:apply-templates>
		</stationresults>
	</xsl:template>
	
	<xsl:template match="classes/*" mode="printSolutions">
		<xsl:param name="station"/>
		<classresults customerclass="{@name}">
			<xsl:call-template name="printSolution">
				<xsl:with-param name="station">
					<xsl:value-of select="$station"/>
				</xsl:with-param>
				<xsl:with-param name="class">
					<xsl:value-of select="@name"/>
				</xsl:with-param>
			</xsl:call-template>
		</classresults>
	</xsl:template>

	
		
	
	<xsl:template name="printSolution">
		<xsl:param name="station"/>
		<xsl:param name="class"/>
		<xsl:for-each select="$solutions/measure[@station=$station and @class=$class]">
					<measure measureType="{@measureType}" upperLimit="{@upperLimit}" meanValue="{@meanValue}" lowerLimit="{@lowerLimit}" successful="{@successful}" analyzedSamples="{@analyzedSamples}" discardedSamples="{@discardedSamples}" precision="{@precision}" alfa="{@alfa}" maxSamples="{@maxSamples}"/>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="blockingRegion" mode="printSolutions_blocking">
		<regionresults region="{@name}">
			<xsl:apply-templates select="../parameters/classes/*" mode="printSolutions_blocking">
				<xsl:with-param name="station">
					<xsl:value-of select="@name"/>
<!--					<xsl:value-of select="concat(@name, '_inputStation')"/> -->
				</xsl:with-param>
			</xsl:apply-templates>
		</regionresults>
	</xsl:template>
	
	<xsl:template match="classes/*" mode="printSolutions_blocking">
		<xsl:param name="station"/>
		<classresults customerclass="{@name}">
			<xsl:call-template name="printSolution_blocking">
				<xsl:with-param name="station">
					<xsl:value-of select="$station"/>
				</xsl:with-param>
				<xsl:with-param name="class">
					<xsl:value-of select="@name"/>
				</xsl:with-param>
			</xsl:call-template>
		</classresults>
	</xsl:template>
	
	<xsl:template name="printSolution_blocking">
		<xsl:param name="station"/>
		<xsl:param name="class"/>
		<xsl:for-each select="$solutions/measure[@station=$station and @class=$class]">
			<xsl:choose>
				<xsl:when test="@measureType='Dropped jobs' or @measureType='Dropped Jobs'">
					<measure>
						<xsl:attribute name="successful"><xsl:value-of select="@successful"/></xsl:attribute>
						<xsl:attribute name="meanValue"><xsl:value-of select="@meanValue"/></xsl:attribute>
						<xsl:attribute name="measureType"><xsl:value-of select="@measureType"/></xsl:attribute>
						<xsl:attribute name="analyzedSamples"><xsl:value-of select="@analyzedSamples"/></xsl:attribute>
						<xsl:attribute name="discardedSamples"><xsl:value-of select="@discardedSamples"/></xsl:attribute>						
					</measure>
				</xsl:when>
				<xsl:when test="@measureType='Region utilization' or @measureType='Region Utilization'">
					<measure>
						<xsl:attribute name="successful"><xsl:value-of select="@successful"/></xsl:attribute>
						<xsl:attribute name="meanValue"><xsl:value-of select="@meanValue"/></xsl:attribute>
						<xsl:attribute name="measureType"><xsl:value-of select="@measureType"/></xsl:attribute>
					</measure>
				</xsl:when>
				<xsl:otherwise>
					<measure measureType="{@measureType}" upperLimit="{@upperLimit}" meanValue="{@meanValue}" lowerLimit="{@lowerLimit}" successful="{@successful}" analyzedSamples="{@analyzedSamples}" discardedSamples="{@discardedSamples}" precision="{@precision}" alfa="{@alfa}" maxSamples="{@maxSamples}"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:for-each>
	</xsl:template>
</xsl:stylesheet>
