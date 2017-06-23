<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="html" version="1.0" encoding="ISO-8859-1" indent="yes"/>
	
	<xsl:variable name="class-name-width">28%</xsl:variable>
	<xsl:variable name="class-type-width">14%</xsl:variable>
	<xsl:variable name="class-rate-width">28%</xsl:variable>
	<xsl:variable name="class-pop-width">28%</xsl:variable>
	<xsl:variable name="station-name-width">50%</xsl:variable>
	<xsl:variable name="station-type-width">50%</xsl:variable>
	<xsl:variable name="param-width">80</xsl:variable>
	
	<xsl:variable name="model" select="model"/>
	
	<!--formats document structure-->
	<xsl:template match="model">
		<html>
			<head>
				<style type="text/css">
				table.main{
					text-align:center;
					border:0px
				}
				.title,.subtitle,.param,.paramhead, td{
					font-family: 8pt "bodoni";
					padding: 1px 5px 1px 5px;
					vertical-align: top
				}
				.title{
					font: bold 24pt; 
					color: #000055
				}
				.paramtitle{
					font: bold 16pt;
					color:#000099;
					background:#fffff9;
				}
				.subtitle{
					font:14pt;
					color:#000077
				}
				table.param, table.stationParam{
					border-color: #000055;
					align:center;
					margin:10px 0px 0px 0px
				}
				table.param{
					width:300px;
				}
				.paramhead{
					font: bold;
					color:ffffff;
					background-color:#000099;
					align:bottom right
				}
				.line1{
					background-color:#ffffff
				}
				.line2{
					background-color:#efefff
				}
								
				</style>
			</head>
			<body>
				<table class="main">
					<tr><th class="title">
					jMVA Model Details<h1 class="subtitle"><xsl:value-of select="description"/></h1>
					</th></tr>
					<tr><td align="center">
					<xsl:for-each select="parameters/*">
						
						<xsl:apply-templates select="." mode="description"/>
						
					</xsl:for-each>
					</td></tr>
					<tr><td align="center">
						<xsl:call-template name="addServiceDemandsTable"/>
					</td></tr>
					<tr><td>
						<xsl:apply-templates select="stations" mode="addLDSettingsTables"/>
					</td></tr>
				</table>
			</body>
		</html>
	</xsl:template>
	
	<!--creates table of classes-->
	<xsl:template match="parameters/classes" mode="description">
		<table class="param" cellspacing="0">
			<tr><th class="paramtitle" colspan="4">
				Classes
			</th></tr>
			<tr class="paramhead">
				<td>Name</td>
				<td>Type</td>
				<td>Population</td>
				<td>Arrival Rate</td>
			</tr>
			<xsl:for-each select="*">
				<tr>
				<xsl:attribute name="class">
					<xsl:if test="position() mod 2=0">line2</xsl:if>
					<xsl:if test="position() mod 2=1">line1</xsl:if>
				</xsl:attribute>
					<td width="{$class-name-width}"><xsl:value-of select="@name"/></td>
					<td width="{$class-type-width}"><xsl:choose>
						<xsl:when test="name(.)='openclass'">open</xsl:when>
						<xsl:when test="name(.)='closedclass'">closed</xsl:when>
					</xsl:choose></td>
					<td width="{$class-pop-width}"><xsl:value-of select="@population"/></td>
					<td width="{$class-rate-width}"><xsl:value-of select="@rate"/></td>
				</tr>
			</xsl:for-each>
		</table>
	</xsl:template>
	
	<!--creates table of stations-->
	<xsl:template match="parameters/stations" mode="description">
		<table class="param" cellspacing="0">
			<tr><th class="paramtitle" colspan="2">
				Stations
			</th></tr>
			<tr class="paramhead">
				<td>Name</td>
				<td>Type</td>
			</tr>
			<xsl:for-each select="*">
				<tr>
				<xsl:attribute name="class">
					<xsl:if test="position() mod 2=0">line2</xsl:if>
					<xsl:if test="position() mod 2=1">line1</xsl:if>
				</xsl:attribute>
					<td width="{$station-name-width}"><xsl:value-of select="@name"/></td>
					<td width="{$station-type-width}"><xsl:choose>
						<xsl:when test="name(.)='listation'">Load Independent</xsl:when>
						<xsl:when test="name(.)='ldstation'">Load Dependent</xsl:when>
						<!--<xsl:when test="name(.)='delaystation'">Delay (Infinite Server)</xsl:when>-->
                        <xsl:when test="name(.)='delaystation'">Delay - Infinite Server</xsl:when>
					</xsl:choose></td>
				</tr>
			</xsl:for-each>
		</table>
	</xsl:template>
	
	
	<!--creates table of service demands-->
	<xsl:template name="addServiceDemandsTable">
		<table class="stationParam" cellspacing="0">
			<tr><th class="paramtitle" colspan="100">
				Service Demands
			</th></tr>
			<tr class="paramhead">
				<td></td>
				<xsl:for-each select="$model/parameters/classes/*">
					<td width="{$param-width}"><xsl:value-of select="@name"/></td>
				</xsl:for-each>
			</tr>
			<xsl:for-each select="$model/parameters/stations/*">
			<xsl:variable name="rowNum"><xsl:value-of select="position()"/></xsl:variable>
				<tr>
					<td class="paramhead" width="{$param-width}">
						<xsl:value-of select="@name"/>
					</td>
					<xsl:choose>
						<xsl:when test="name(.)='listation' or name(.)='delaystation'">
							<xsl:for-each select="servicetimes/servicetime">
								<xsl:variable name="classname" select="@customerclass"/>
								<xsl:variable name="visit" select="../../visits/visit[@customerclass=$classname]"/>
								<td>
								<xsl:attribute name="class">
									<xsl:if test="number($rowNum) mod 2=0">line2</xsl:if>
									<xsl:if test="number($rowNum) mod 2=1">line1</xsl:if>
								</xsl:attribute>
									<xsl:value-of select="number($visit)*number(.)"/>
								</td>
							</xsl:for-each>
						</xsl:when>
						<xsl:when test="name(.)='ldstation'">
							<xsl:for-each select="servicetimes/servicetimes">
								<td>
								<xsl:attribute name="class">
									<xsl:if test="number($rowNum) mod 2=0">line2</xsl:if>
									<xsl:if test="number($rowNum) mod 2=1">line1</xsl:if>
								</xsl:attribute>
									<xsl:call-template name="substituteSemiColonInLD">
									<xsl:with-param name="rightstring"><xsl:value-of select="."/></xsl:with-param>
									</xsl:call-template>
								</td>
							</xsl:for-each>
						</xsl:when>
					</xsl:choose>
				</tr>
			</xsl:for-each>
		</table>
	</xsl:template>
		
	<xsl:template name="substituteSemiColonInLD">
	<xsl:param name="rightstring"/>
		<xsl:if test="$rightstring!=''">
			<xsl:value-of select="substring-before(string($rightstring),';')"/>
			<xsl:if test="false = contains(string($rightstring),';')">
				<xsl:value-of select="$rightstring"/>
			</xsl:if>
			<xsl:if test="contains(string($rightstring),';') or substring-after(string($rightstring),';')!=''">
				<br/>
			</xsl:if>
			<xsl:call-template name="substituteSemiColonInLD">
				<xsl:with-param name="rightstring"><xsl:value-of select="substring-after(string($rightstring),';')"/></xsl:with-param>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
	
</xsl:stylesheet>
