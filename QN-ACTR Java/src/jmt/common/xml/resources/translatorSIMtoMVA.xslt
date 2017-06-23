<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" version="1.0" encoding="ISO-8859-1" indent="yes"/>
	
	<xsl:template match="/">
	<xsl:processing-instruction name="xml-stylesheet">"type="text/xsl" href="translatorMVAtoSIM.xslt"</xsl:processing-instruction>
		<model>
		<xsl:attribute name="xsi:noNamespaceSchemaLocation" namespace="http://www.w3.org/2001/XMLSchema-instance">exactmodel.xsd</xsl:attribute>
			<parameters>
				<xsl:apply-templates select="sim"/>
			</parameters>
		</model>
	</xsl:template>
	
	<xsl:template match="sim">
		<xsl:call-template name="createClasses"/>
		<xsl:call-template name="createStations"/>
	</xsl:template>
	
	<xsl:template name="createClasses">
		<classes>
		<xsl:attribute name="number"><xsl:value-of select="count(userClass)"/></xsl:attribute>
		<xsl:for-each select="userClass">
			<xsl:choose>
				<xsl:when test="@type='closed' or @type='Closed'">
					<closedclass>
					<xsl:attribute name="name"><xsl:value-of select="./@name"/></xsl:attribute>
					<xsl:attribute name="population"><xsl:value-of select="./@customers"/></xsl:attribute>
					</closedclass>
				</xsl:when>
				<xsl:when test="@type='open' or @type='Open'">
					<openclass>
					<xsl:attribute name="name"><xsl:value-of select="./@name"/></xsl:attribute>
					<xsl:attribute name="rate">
						<xsl:call-template name="extractOpenClassRate">
							<xsl:with-param name="refNode"><xsl:value-of select="@referenceSource"/></xsl:with-param>
							<xsl:with-param name="refUClass"><xsl:value-of select="@name"/></xsl:with-param>
						</xsl:call-template> 
					</xsl:attribute>
					</openclass>
				</xsl:when>
			</xsl:choose>
		</xsl:for-each>
		</classes>
	</xsl:template>
	
	<xsl:template name="createStations">
		<stations>
		<xsl:attribute name="number"><xsl:value-of select="count(node[section/@className='Delay' or section/@className='Server' or section/@className='ServerLD'])"/></xsl:attribute>
		<xsl:for-each select="node">
		<xsl:for-each select="section">
			<xsl:choose>
				<xsl:when test="@className='Delay'">
					<delaystation>
						<xsl:attribute name="name"><xsl:value-of select="../@name"/></xsl:attribute>
						<xsl:call-template name="writeStationParameters">
							<xsl:with-param name="stationType">LI</xsl:with-param>
						</xsl:call-template>
					</delaystation>
				</xsl:when>
				<xsl:when test="@className='Server'">
					<listation>
					<xsl:attribute name="name"><xsl:value-of select="../@name"/></xsl:attribute>
						<xsl:call-template name="writeStationParameters">
							<xsl:with-param name="stationType">LI</xsl:with-param>
						</xsl:call-template>
					</listation>
				</xsl:when>
				<xsl:when test="@className='ServerLD'">
					<ldstation>
					<xsl:attribute name="name"><xsl:value-of select="../@name"/></xsl:attribute>
						<xsl:call-template name="writeStationParameters">
							<xsl:with-param name="stationType">LD</xsl:with-param>
						</xsl:call-template>
					</ldstation>
				</xsl:when>
			</xsl:choose>
		</xsl:for-each>
		</xsl:for-each>
		</stations>
	</xsl:template>

	<xsl:template name="writeStationParameters">
	<xsl:param name="stationType"/>
		<servicetimes>
			<xsl:for-each select="parameter[@name='ServiceStrategy']/*">
				<xsl:if test="name(.)='refClass'">
					<xsl:call-template name="addServiceTime">
						<xsl:with-param name="index"><xsl:value-of select="position()"/></xsl:with-param>
						<xsl:with-param name="stationType"><xsl:value-of select="$stationType"/></xsl:with-param>
						<xsl:with-param name="userClassName"><xsl:value-of select="."/></xsl:with-param>
					</xsl:call-template>
				</xsl:if>
			</xsl:for-each>
		</servicetimes>
		<visits>
			<xsl:for-each select="parameter[@name='numberOfVisits']/*">
				<xsl:if test="name(.)='refClass'">
					<xsl:call-template name="addVisit">
						<xsl:with-param name="index"><xsl:value-of select="position()"/></xsl:with-param>
						<xsl:with-param name="userClassName"><xsl:value-of select="."/></xsl:with-param>
					</xsl:call-template>
				</xsl:if>
			</xsl:for-each>
		</visits>
	</xsl:template>
	
	<!--adds a servicetime child to the current node. Its name is 'servicetime' if the station is load independent or a delay station, otherwise 
	its name is servicetimes, according with the exactmodel.xsd definition. Input parameters are:
	- Index: index of the refClass child which this servicetime refers to.
	- stationType: type of this station, li or ld as explained above.
	- userClassName: name of the userclass this service time refers to.-->
	<xsl:template name="addServiceTime">
	<xsl:param name="index"/>	
	<xsl:param name="stationType"/>	
	<xsl:param name="userClassName"/>
		<xsl:choose>
			<xsl:when test="$stationType='LI'">
				<servicetime>
					<xsl:attribute name="customerclass">
						<xsl:value-of select="$userClassName"/>
					</xsl:attribute>
					<xsl:value-of select="1 div ../*[number($index)+1]/subParameter/subParameter[@name='lambda']/value"/>
				</servicetime>
			</xsl:when>
			<xsl:when test="$stationType='LD'">
				<servicetimes>
					<xsl:attribute name="customerclass">
						<xsl:value-of select="$userClassName"/>
					</xsl:attribute>
					<xsl:value-of select="../*[number($index)+1]/subParameter/subParameter[@name='lambda']/value"/>
				</servicetimes>
			</xsl:when>
		</xsl:choose>	
	</xsl:template>
	
	<!--adds a visit child to the current node. Input parameters are index of the refclass child of the class this visit value refers to and
	userClassName, the name of that class. this template uses index of the refClass child to search the next child ( which should be a 
	parameter child, according to the xsd definition of the xml model) that must contain the parameter relative to the class.-->
	<xsl:template name="addVisit">
	<xsl:param name="index"/>	
	<xsl:param name="userClassName"/>
		<visit>
			<xsl:attribute name="customerclass">
				<xsl:value-of select="$userClassName"/>
			</xsl:attribute>
			<xsl:value-of select="../*[number($index)+1]/value"/>
		</visit>
	</xsl:template> 
	
	<!-- searches for the refnode of this openclass in order to extract the value of the arrival rate, which is generally the lambda para-
	meter of the -->
	<xsl:template name="extractOpenClassRate">
	<xsl:param name="refNode"/>
	<xsl:param name="refUClass"/>
		<xsl:for-each select="../node[@name=$refNode]/section[@className='RandomSource']/parameter[@name='ServiceStrategy']/*">
			<xsl:if test=".=$refUClass">
				<xsl:call-template name="getNextNode">
				<xsl:with-param name="index"><xsl:value-of select="number(position())"/></xsl:with-param>
				</xsl:call-template>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template name="getNextNode">
	<xsl:param name="index"/>
		<xsl:value-of select="../*[number($index)+1]/subParameter/subParameter[@name='lambda']/value"/>
	</xsl:template>
	
</xsl:stylesheet>
