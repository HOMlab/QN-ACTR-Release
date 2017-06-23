<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" version="1.0" encoding="ISO-8859-1" indent="yes" media-type="text/xml"/>
	
	<xsl:variable name="stations" select="model/parameters/stations"/>
	
	<xsl:variable name="defaultSourceName">Source0</xsl:variable>
	<xsl:variable name="defaultSinkName">Sink0</xsl:variable>
	<xsl:variable name="defaultTerminalName">Terminal0</xsl:variable>
	
	<xsl:key name="sumOfVisits" match="model/parameters/stations/*/visits/visit" use="@customerclass"/>
	
	<xsl:template match="model">
		<sim debug="false" name="MVA-Converted"> 
		<xsl:attribute name="xsi:noNamespaceSchemaLocation" namespace="http://www.w3.org/2001/XMLSchema-instance">SIMmodeldefinition.xsd</xsl:attribute>
			<xsl:apply-templates select="parameters"/>
			<xsl:call-template name="createMeasures"/>
			<xsl:call-template name="createConnections"/>
			<xsl:call-template name="createRegions"/>
		</sim>
	</xsl:template>
	
	<xsl:template match="parameters">
<xsl:comment>
<xsl:for-each select="classes/*">
			Totale visite classe <xsl:value-of select="@name"/>: <xsl:copy-of select="sum(key('sumOfVisits',@name))"/>;
</xsl:for-each>
</xsl:comment>		
		<xsl:apply-templates select="classes/closedclass"/>
		<xsl:apply-templates select="classes/openclass"/>
		<!--the following templates check for presence of open/closed classes and eventually 
		create stations which are required by the simulator to work, but are not defined in the mva model-->
		<xsl:apply-templates select="classes/closedclass[1]" mode="checkClosedPresence"/>
		<xsl:apply-templates select="classes/openclass[1]" mode="checkOpenPresence"/>
		<xsl:apply-templates select="stations/listation"/>
		<xsl:apply-templates select="stations/ldstation"/>
		<xsl:apply-templates select="stations/delaystation"/>
	</xsl:template>
	
	<!--Creates connections for this model. As all of the connections must be parallelized, if there are closed classes all of the stations must be 
	      connected to the terminal as precessor and successor nodes. If there are openclasses all of the stations must have a source as a 
	      precessor node and a sink as a successor node.-->
	<xsl:template name="createConnections">
		<xsl:for-each select="parameters/stations/*">
			<xsl:if test="count(../../classes/openclass)!=0">
				<connection source="{$defaultSourceName}" target="{@name}"/>
				<connection source="{@name}" target="{$defaultSinkName}"/>
			</xsl:if>
			<xsl:if test="count(../../classes/closedclass)!=0">
				<connection source="{$defaultTerminalName}" target="{@name}"/>
				<connection source="{@name}" target="{$defaultTerminalName}"/>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>

	<!--Creates all of the possible measures on each station for each class.-->
	<xsl:template name="createMeasures">
		<xsl:for-each select="parameters/stations/*">
			<xsl:call-template name="createStationMeasures">
			<xsl:with-param name="station"><xsl:value-of select="@name"/></xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template name="createStationMeasures">
	<xsl:param name="station"/>
		<xsl:for-each select="../../classes/*">
			<xsl:call-template name="createMeasure">
			<xsl:with-param name="station"><xsl:value-of select="$station"/></xsl:with-param>
			<xsl:with-param name="class"><xsl:value-of select="@name"/></xsl:with-param>
			<xsl:with-param name="measureType">Queue length</xsl:with-param>
			<xsl:with-param name="s_measureType">Q</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="createMeasure">
			<xsl:with-param name="station"><xsl:value-of select="$station"/></xsl:with-param>
			<xsl:with-param name="class"><xsl:value-of select="@name"/></xsl:with-param>
			<xsl:with-param name="measureType">Throughput</xsl:with-param>
			<xsl:with-param name="s_measureType">X</xsl:with-param>
			</xsl:call-template>
			<!--
			<xsl:call-template name="createMeasure">
			<xsl:with-param name="station"><xsl:value-of select="$station"/></xsl:with-param>
			<xsl:with-param name="class"><xsl:value-of select="@name"/></xsl:with-param>
			<xsl:with-param name="measureType">Response time</xsl:with-param>
			<xsl:with-param name="s_measureType">R</xsl:with-param>
			</xsl:call-template>
			-->
			<xsl:call-template name="createMeasure">
			<xsl:with-param name="station"><xsl:value-of select="$station"/></xsl:with-param>
			<xsl:with-param name="class"><xsl:value-of select="@name"/></xsl:with-param>
			<xsl:with-param name="measureType">Residence time</xsl:with-param>
			<xsl:with-param name="s_measureType">W</xsl:with-param>
			</xsl:call-template>
			<xsl:call-template name="createMeasure">
			<xsl:with-param name="station"><xsl:value-of select="$station"/></xsl:with-param>
			<xsl:with-param name="class"><xsl:value-of select="@name"/></xsl:with-param>
			<xsl:with-param name="measureType">Utilization</xsl:with-param>
			<xsl:with-param name="s_measureType">U</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template name="createMeasure">
	<xsl:param name="station"/>
	<xsl:param name="class"/>
	<xsl:param name="s_measureType"/>
	<xsl:param name="measureType"/>
		<measure name="{$s_measureType}_{$class}_{$station}" alpha="0.1" precision="0.1" type="{$measureType}" referenceNode="{$station}" referenceUserClass="{$class}"/> 
	</xsl:template>
	
	<xsl:template name="createRegions">
		<xsl:copy-of select="blockingRegion"/>
	</xsl:template>
	
	<xsl:template match="closedclass">
		<userClass>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:attribute name="type">closed</xsl:attribute>
			<xsl:attribute name="priority">0</xsl:attribute>
			<xsl:attribute name="referenceSource"><xsl:value-of select="$defaultTerminalName"/></xsl:attribute>
			<xsl:attribute name="customers"><xsl:value-of select="@population"/></xsl:attribute>
		</userClass>
	</xsl:template>
	
	<xsl:template match="openclass">
		<userClass>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:attribute name="type">open</xsl:attribute>
			<xsl:attribute name="priority">0</xsl:attribute>
			<xsl:attribute name="referenceSource"><xsl:value-of select="$defaultSourceName"/></xsl:attribute>
		</userClass>
	</xsl:template>
	
	<xsl:template match="closedclass" mode="checkClosedPresence">
		<xsl:call-template name="createTerminal"/>
	</xsl:template>

	<xsl:template match="openclass" mode="checkOpenPresence">
		<xsl:call-template name="createSource"/>
		<xsl:call-template name="createSink"/>
	</xsl:template>
	
	<!--templates for creation of stations that have been directly defined in mva model-->
	<xsl:template match="listation">
		<node>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:call-template name="createQueueSection"/>
			<section className="Server">
				<parameter classPath="java.lang.Integer" name="maxJobs">
					<value>1</value>
				</parameter>
				<parameter array="true" classPath="java.lang.Integer" name="numberOfVisits">
					<xsl:apply-templates select="visits/visit"/>
				</parameter>
				<parameter array="true" classPath="jmt.engine.NetStrategies.ServiceStrategy" name="ServiceStrategy">
					<xsl:apply-templates select="servicetimes/servicetime"/>
				</parameter>
			</section>
			<xsl:call-template name="createRouterSection">
			<xsl:with-param name="stationName"><xsl:value-of select="@name"/></xsl:with-param>
			</xsl:call-template>
		</node>
	</xsl:template>
	
	<xsl:template match="ldstation">
		<node>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:call-template name="createQueueSection"/>
			<section className="ServerLD">
				<parameter classPath="java.lang.Integer" name="maxJobs">
					<value>1</value>
				</parameter>
				<parameter array="true" classPath="java.lang.Integer" name="numberOfVisits">
					<xsl:apply-templates select="visits/visit"/>
				</parameter>
				<parameter array="true" classPath="jmt.engine.NetStrategies.ServiceStrategy" name="ServiceStrategy">
					<xsl:apply-templates select="servicetimes/servicetimes"/>
				</parameter>
			</section>
			<xsl:call-template name="createRouterSection">
			<xsl:with-param name="stationName"><xsl:value-of select="@name"/></xsl:with-param>
			</xsl:call-template>
		</node>
	</xsl:template>
	
	<xsl:template match="delaystation">
		<node>
			<xsl:attribute name="name"><xsl:value-of select="@name"/></xsl:attribute>
			<xsl:call-template name="createQueueSection"/>
			<section className="Delay">
				<parameter array="true" classPath="jmt.engine.NetStrategies.ServiceStrategy" name="ServiceStrategy">
					<xsl:apply-templates select="servicetimes/servicetime" mode="delayServiceTime"/>
				</parameter>
			</section>
			<xsl:call-template name="createRouterSection">
			<xsl:with-param name="stationName"><xsl:value-of select="@name"/></xsl:with-param>
			</xsl:call-template>
		</node>
	</xsl:template>
	
	<!--templates for creation of stations whose presence is required by simulator but not by the mva engine-->
	<xsl:template name="createSource">
		<node name="{$defaultSourceName}">
			<section className="RandomSource">
				<parameter array="true" classPath="jmt.engine.NetStrategies.ServiceStrategy" name="ServiceStrategy">
				<xsl:for-each select="//model/parameters/classes/*">
					<xsl:choose>
						<xsl:when test="name(.)='openclass'">
							<refClass><xsl:value-of select="@name"/></refClass>
							<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategy">
								<subParameter classPath="jmt.engine.random.Exponential" name="Exponential"/>
								<subParameter classPath="jmt.engine.random.ExponentialPar" name="distrPar">
									<subParameter classPath="java.lang.Double" name="lambda">
										<!--<value><xsl:value-of select="string(@rate) * sum(key('sumOfVisits',@name))"/></value>-->
										<xsl:variable name="rate_noexp">
											<xsl:call-template name="formatExpNumber">
												<xsl:with-param name="number"><xsl:value-of select="string(@rate)"/></xsl:with-param>
											</xsl:call-template>
										</xsl:variable>
										<value>										
											<xsl:value-of select="$rate_noexp * sum(key('sumOfVisits',@name))"/>
										</value>
									</subParameter>	
								</subParameter>
							</subParameter>
						</xsl:when>
						<xsl:otherwise>
							<refClass><xsl:value-of select="@name"/></refClass>
							<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategy">
								<value>null</value>
							</subParameter>
						</xsl:otherwise>
					</xsl:choose>
				</xsl:for-each>
				</parameter>
			</section>
			<section className="ServiceTunnel"/>
			<xsl:call-template name="createRouterSection">
			<xsl:with-param name="stationName"><xsl:value-of select="$defaultSourceName"/></xsl:with-param>
			</xsl:call-template>
		</node>
	</xsl:template>
	
	<xsl:template name="createSink">
		<node name="{$defaultSinkName}">
			<section className="JobSink"/>
		</node>
	</xsl:template>
	
	<xsl:template name="createTerminal">
		<node name="{$defaultTerminalName}">
			<section className="Terminal">
				<parameter array="true" classPath="java.lang.Integer" name="numberOfJobs">
				<xsl:for-each select="//model/parameters/classes/*">
					<refClass><xsl:value-of select="@name"></xsl:value-of></refClass>
					<subParameter classPath="java.lang.Integer" name="numberOfJobs">
						<value>
							<xsl:choose>
								<xsl:when test="name(.)='closedclass'"><xsl:value-of select="@population"/></xsl:when>
								<xsl:otherwise>-1</xsl:otherwise>
							</xsl:choose>
						</value>
					</subParameter>
				</xsl:for-each>
				</parameter>
			</section>
			<section className="ServiceTunnel"/>
			<xsl:call-template name="createRouterSection">
			<xsl:with-param name="stationName"><xsl:value-of select="$defaultTerminalName"/></xsl:with-param>
			</xsl:call-template>
		</node>
	</xsl:template>
	
	<!--templates for creation of node sections. Accepts name of station as parameter.-->
	<xsl:template name="createRouterSection">
	<xsl:param name="stationName"/>
		<section className="Router">
			<xsl:choose>
				<!--If this station is a source or a terminal, routing section must route jobs to the servers with probabilities according to the 
				     number of visits of each class defined on each server in the mva model.-->
				<xsl:when test="$stationName=$defaultSourceName or $stationName=$defaultTerminalName">
					<parameter array="true" classPath="jmt.engine.NetStrategies.RoutingStrategy" name="RoutingStrategy">
						<xsl:apply-templates select="../../classes/*" mode="MultipleEmpiricalRouting"/>
					</parameter>
				</xsl:when>
				<!--If neither all of the classes are open, nor are all closed, routing strategy must route all of the
				openclasses to the sink that will be created, and all of the closed ones to the terminal station. This must be specified with an 
				empirical strategy. Otherwise also a random routingstrategy can be used, as this station is link only to one other.-->
				<xsl:when test="count(../../classes/openclass)=0 or count(../../classes/closedclass)=0">
					<parameter array="true" classPath="jmt.engine.NetStrategies.RoutingStrategy" name="RoutingStrategy">
						<xsl:apply-templates select="../../classes/*" mode="ignoreValueRouting"/>
					</parameter>
				</xsl:when>
				<!--If this station is a li or a ld ora a delay station and there are both open and closed classes, the routing section must route 
				     openclasses to the sink, closedclasses to the terminal-->
				<xsl:when test="name(.)='ldstation' or name(.)='listation' or name(.)='delaystation' ">
					<parameter array="true" classPath="jmt.engine.NetStrategies.RoutingStrategy" name="RoutingStrategy">
						<xsl:apply-templates select="../../classes/*" mode="EmpiricalRouting"/>
					</parameter>
				</xsl:when>
			</xsl:choose>
		</section>
	</xsl:template>	

	<xsl:template name="createQueueSection">
		<section className="Queue">
			<parameter classPath="java.lang.Integer" name="size">
				<value>-1</value>
			</parameter>
			<parameter classPath="java.lang.Boolean" name="drop">
				<value>false</value>
			</parameter>
			<parameter classPath="jmt.engine.NetStrategies.QueueGetStrategies.FCFSstrategy" name="FCFSstrategy"/>
			<parameter array="true" classPath="jmt.engine.NetStrategies.QueuePutStrategy" name="NetStrategy">
				<xsl:apply-templates select="../../classes/*" mode="ignoreValueQueueing"/>
			</parameter>
		</section>
	</xsl:template>
	
	<!--templates for definition of stations' parameters - service times-->
	<xsl:template match="servicetime">
			<refClass>
				<xsl:value-of select="@customerclass"/>
			</refClass>
			<xsl:choose>
				<xsl:when test="number(.)=0">
					<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ZeroServiceTimeStrategy" name="ZeroTimeServiceStrategy"/>
				</xsl:when>
				<xsl:otherwise>
					<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategy">
						<subParameter classPath="jmt.engine.random.Exponential" name="Exponential"/>
						<subParameter classPath="jmt.engine.random.ExponentialPar" name="distrPar">
							<subParameter classPath="java.lang.Double" name="lambda">
								<value><xsl:value-of select="1 div number(.)"/></value>
							</subParameter>
						</subParameter>
					</subParameter>
				</xsl:otherwise>
			</xsl:choose>
	</xsl:template>
	
	<xsl:template match="servicetime" mode="delayServiceTime">
			<refClass>
				<xsl:value-of select="@customerclass"/>
			</refClass>
			<xsl:choose>
				<xsl:when test="number(.)=0">
					<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategy">
						<subParameter classPath="jmt.engine.random.ConstantDistr" name="ConstantDistr"/>
						<subParameter classPath="jmt.engine.random.ConstantDistrPar" name="distrPar">
							<subParameter classPath="java.lang.Double" name="const">
								<value>0</value>
							</subParameter>
						</subParameter>
					</subParameter>
				</xsl:when>
				<xsl:otherwise>
					<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategy">
						<subParameter classPath="jmt.engine.random.Exponential" name="Exponential"/>
						<subParameter classPath="jmt.engine.random.ExponentialPar" name="distrPar">
							<subParameter classPath="java.lang.Double" name="lambda">
								<value><xsl:value-of select="1 div number(.)"/></value>
							</subParameter>
						</subParameter>
					</subParameter>
				</xsl:otherwise>
			</xsl:choose>
	</xsl:template>
	
	<xsl:template match="servicetimes">
			<refClass>
				<xsl:value-of select="@customerclass"/>
			</refClass>
			<subParameter classPath="jmt.engine.NetStrategies.ServiceStrategies.ServiceTimeStrategy" name="TimeServiceStrategyLD">
				<subParameter classPath="jmt.engine.random.Exponential" name="Exponential"/>
				<subParameter classPath="jmt.engine.random.ExponentialPar" name="distrPar">
					<subParameter classPath="java.lang.String" name="lambda">
						<value>
							<xsl:value-of select="."/>
						</value>
					</subParameter>
				</subParameter>
			</subParameter>
	</xsl:template>
	
	<!--initialization of station parameters - number of visits. All of the visits must be setted to 1 because the mva model's system of visist is 
	     managed out with a proper combination of node topology and routing probabilities. In particular the topologic configuration of the model
	     is described by a set of parallel connections, while routing probabilities are setted for each class and each station as the ratio between
	     the number of visits of the successor node's station and the sum of number of visits above the whole set of successor stations 
	     separately for each class-->
	<xsl:template match="visit">
			<refClass>
				<xsl:value-of select="@customerclass"/>
			</refClass>
			<subParameter classPath="java.lang.Integer" name="numberOfVisits">
				<value>1</value>
			</subParameter>
	</xsl:template>
	
	<!--theese three templates create routing strategy parameters for each class. The first creates random routing strategy in case there is only
	     one successor node or probability for each successor node may be not specified. The second sets routing strategy when successor nodes 
	     are terminal and sink: openclasses must be routed to the sink, the closed ones to the terminal. The third finally routes jobs to each server 
	     assigning different probabilities according with the number of visits defined for each server in the mva model.-->
	<xsl:template match="classes/*" mode="ignoreValueRouting">
			<refClass>
				<xsl:value-of select="@name"/>
			</refClass>
			<subParameter classPath="jmt.engine.NetStrategies.RoutingStrategies.RandomStrategy" name="RandomStrategy"/>
	</xsl:template>
	
	<xsl:template match="classes/*" mode="EmpiricalRouting">
			<refClass>
				<xsl:value-of select="@name"/>
			</refClass>
			<subParameter classPath="jmt.engine.NetStrategies.RoutingStrategies.EmpiricalStrategy" name="EmpiricalStrategy">
				<xsl:call-template name="createEmpiricalEntries"/>
			</subParameter>  
	</xsl:template>

	<xsl:template match="classes/*" mode="MultipleEmpiricalRouting">
			<refClass>
				<xsl:value-of select="@name"/>
			</refClass>
			<subParameter classPath="jmt.engine.NetStrategies.RoutingStrategies.EmpiricalStrategy" name="EmpiricalStrategy">
				<xsl:call-template name="createMultipleEmpiricalEntries"/>
			</subParameter>  
	</xsl:template>
	
	<!--Creates empirical entries for the Empirical strategy subparameter element.-->
	<xsl:template name="createEmpiricalEntries">
		<subParameter array="true" classPath="jmt.engine.random.EmpiricalEntry" name="EmpiricalEntry"> 
		<xsl:call-template name="createEmpiricalEntry">
			<xsl:with-param name="stationName"><xsl:value-of select="$defaultTerminalName"/></xsl:with-param>
			<xsl:with-param name="probability">
				<xsl:choose>
					<xsl:when test="name(.)='openclass'">0.0</xsl:when>
					<xsl:when test="name(.)='closedclass'">1.0</xsl:when>
				</xsl:choose>
			</xsl:with-param>
		</xsl:call-template>
		<xsl:call-template name="createEmpiricalEntry">
			<xsl:with-param name="stationName"><xsl:value-of select="$defaultSinkName"/></xsl:with-param>
			<xsl:with-param name="probability">
				<xsl:choose>
					<xsl:when test="name(.)='openclass'">1.0</xsl:when>
					<xsl:when test="name(.)='closedclass'">0.0</xsl:when>
				</xsl:choose>
			</xsl:with-param>
		</xsl:call-template>
		</subParameter>
	</xsl:template>
	
	<!--Creates empirical entries for the Empirical strategy subparameter element for each server.-->
	<xsl:template name="createMultipleEmpiricalEntries">
	<xsl:param name="className"><xsl:value-of select="@name"/></xsl:param>
		<subParameter array="true" classPath="jmt.engine.random.EmpiricalEntry" name="EmpiricalEntry">
		<xsl:for-each select="$stations/*">
			<xsl:call-template name="createEmpiricalEntry">
				<xsl:with-param name="stationName"><xsl:value-of select="@name"/></xsl:with-param>
				<xsl:with-param name="probability">
					<xsl:value-of select="number(visits/visit[@customerclass=$className] div sum(key('sumOfVisits',$className)))"/>
				</xsl:with-param>
			</xsl:call-template>
		</xsl:for-each> 
		</subParameter>
	</xsl:template>
	
	<!--Creates a single empirycal entry by specifying which station the probability must refer to and the value of the probability itself-->
	<xsl:template name="createEmpiricalEntry">
	<xsl:param name="stationName"/>
	<xsl:param name="probability"/>
		<subParameter array="false" classPath="jmt.engine.random.EmpiricalEntry" name="EmpiricalEntry"> 
			<subParameter classPath="java.lang.String" name="stationName">
				<value>
					<xsl:value-of select="$stationName"/>
				</value>
			</subParameter> 
			<subParameter classPath="java.lang.Double" name="probability">
				<value>
					<xsl:value-of select="$probability"/>
				</value>
			</subParameter>
		</subParameter>
	</xsl:template>
	
	<xsl:template match="classes/*" mode="ignoreValueQueueing">
		<refClass>
			<xsl:value-of select="@name"/>
		</refClass>
		<subParameter classPath="jmt.engine.NetStrategies.QueuePutStrategies.TailStrategy" name="TailStrategy"/>
	</xsl:template>
	
	<xsl:template name="formatExpNumber">
	<xsl:param name="number"/>
		<xsl:if test="contains($number, 'E')">
		<xsl:variable name="mantissa" select="substring-before($number,'E')"/>
		<xsl:variable name="exponent" select="substring-after($number,'E')"/>
			<xsl:choose>
			<!--If exponent is equal to 1-->
				<xsl:when test="number($exponent) = 1"><xsl:value-of select="$mantissa*10"/></xsl:when>
			<!--If exponent is equal to -1-->
				<xsl:when test="number($exponent) = -1"><xsl:value-of select="number($mantissa) div 10"/></xsl:when>
			<!--If exponent is equal to 0-->
				<xsl:when test="number($exponent) = 0"><xsl:value-of select="$mantissa"/></xsl:when>
			<!--If exponent is positive-->
				<xsl:when test="number($exponent) &gt; 1">
					<!--recurse with a number that has an exponent decremented by 1 and mantissa multiplied by 10-->
					<xsl:call-template name="formatExpNumber">
						<xsl:with-param name="number" select="
							concat(
								concat(
									string(number($mantissa)*10),'E'
								),
								string(number($exponent)-1)
							)
						"/>
					</xsl:call-template>
				</xsl:when>
			<!--If exponent is negative-->
				<xsl:when test="number($exponent) &lt; -1">
					<!--recurse with a number that has an exponent incremented by 1 and mantissa divided by 10-->
					<xsl:call-template name="formatExpNumber">
						<xsl:with-param name="number" select="
							concat(
								concat(
									string(number($mantissa) div 10),'E'
								),
								string(number($exponent)+1)
							)
						"/>
					</xsl:call-template>
				</xsl:when>
			</xsl:choose>
		</xsl:if>
		<xsl:if test="false=contains($number,'E')">
			<xsl:value-of select="$number"/>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
