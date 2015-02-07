package net.meisen.dissertation.config.xslt;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.impl.auth.AllAccessAuthManager;
import net.meisen.dissertation.impl.cache.IdsOnlyDataRecordCache;
import net.meisen.dissertation.impl.cache.MemoryBitmapCache;
import net.meisen.dissertation.impl.cache.MemoryFactDescriptorModelSetCache;
import net.meisen.dissertation.impl.cache.MemoryIdentifierCache;
import net.meisen.dissertation.impl.cache.MemoryMetaDataCache;
import net.meisen.dissertation.impl.dataretriever.CsvDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetriever;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.measures.Count;
import net.meisen.dissertation.impl.measures.CountFinished;
import net.meisen.dissertation.impl.measures.CountStarted;
import net.meisen.dissertation.impl.measures.Max;
import net.meisen.dissertation.impl.measures.Mean;
import net.meisen.dissertation.impl.measures.Median;
import net.meisen.dissertation.impl.measures.Min;
import net.meisen.dissertation.impl.measures.Mode;
import net.meisen.dissertation.impl.measures.Sum;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.MapperFactory;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.data.DataModel;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.idfactories.IIdsFactory;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.IDataRecordFactory;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularityFactory;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.dissertation.model.time.mapper.BaseMapperFactory;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition;
import net.meisen.dissertation.server.sessions.SessionManager;
import net.meisen.general.genmisc.types.Classes;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Strings;
import net.meisen.general.sbconfigurator.api.IConfiguration;

/**
 * This class presents the default values used within the XSLT process.
 * 
 * @author pmeisen
 * 
 */
public class DefaultValues {
	/**
	 * The name of the module created by the XSLT process which contains the
	 * created {@code MetaDataModel}.
	 * 
	 * @see MetaDataModel
	 */
	public static final String METADATAMODEL_ID = "metaDataModel";
	/**
	 * The name of the module created by the XSLT process which contains the
	 * define meta-data.
	 */
	public static final String METADATACOLLECTION_ID = "metaDataCollection";
	/**
	 * The name of the module created by the XSLT process which contains the
	 * created {@code IntervalModel}.
	 * 
	 * @see IntervalModel
	 */
	public static final String INTERVALMODEL_ID = "intervalModel";
	/**
	 * The name of the module created by the XSLT process which handles
	 * dimensions.
	 */
	public static final String DIMENSIONMODEL_ID = "dimensionModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code DataModel}.
	 * 
	 * @see DataModel
	 */
	public static final String DATAMODEL_ID = "dataModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code DataStructure}.
	 * 
	 * @see DataStructure
	 */
	public static final String DATASTRUCTURE_ID = "dataStructure";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code TimeIntervalDataAnalyzerModel}.
	 * 
	 * @see TidaModel
	 */
	public static final String TIDAMODEL_ID = "timeIntervalDataAnalyzerModel";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code IndexFactory}.
	 * 
	 * @see BaseIndexFactory
	 */
	public static final String INDEXFACTORY_ID = "indexFactory";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code BaseMapperFactory}.
	 * 
	 * @see BaseMapperFactory
	 */
	public static final String MAPPERFACTORY_ID = "mapperFactory";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code TimeGranularityFactory}.
	 * 
	 * @see ITimeGranularityFactory
	 */
	public static final String GRANULARITYFACTORY_ID = "granularityFactory";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code DataRecordFactory}.
	 * 
	 * @see IDataRecordFactory
	 */
	public static final String DATARECORDFACTORY_ID = "dataRecordFactory";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code AggregationFunctionHandler}.
	 * 
	 * @see AggregationFunctionHandler
	 */
	public static final String AGGREGATIONFUNCTIONHANDLER_ID = "aggFunctionHandler";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code SessionManager}.
	 * 
	 * @see SessionManager
	 */
	public static final String SESSIONMANAGER_ID = "sessionManager";

	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code QueryFactory}.
	 * 
	 * @see IQueryFactory
	 */
	public static final String QUERYFACTORY_ID = "queryFactory";
	/**
	 * The name of the module created by the XSLT process, which contains the
	 * created {@code TimelineDefinition}.
	 * 
	 * @see TimelineDefinition
	 */
	public static final String TIMELINEDEFINITION_ID = "timelineDefinition";
	/**
	 * Id of the {@code BasePersistor} instance to be used.
	 */
	public static final String PERSISTOR_ID = "persistorId";
	/**
	 * Id of the {@code TidaModelHandler} to be used.
	 */
	public static final String MODELHANDLER_ID = "modelHandlerId";
	/**
	 * Id of the module to load dimensions
	 */
	public static final String DIMENSIONHANDLER_ID = "dimensionHandlerId";
	/**
	 * Id of the {@code IdentifierCache} to be used.
	 */
	public static final String IDENTIFIERCACHE_ID = "identifierCacheId";
	/**
	 * Id of the {@code MetaDataCache} to be used.
	 */
	public static final String METADATACACHE_ID = "metaDataCacheId";
	/**
	 * Id of the {@code BitmapCache} to be used.
	 */
	public static final String BITMAPCACHE_ID = "bitmapCacheId";
	/**
	 * Id of the {@code FactDescriptorModelSetCache} to be used.
	 */
	public static final String FACTSETSCACHE_ID = "factSetsCacheId";
	/**
	 * Id of the {@code DataRecordCache} to be used.
	 */
	public static final String DATARECORDCACHE_ID = "dataRecordCacheId";
	/**
	 * Id of the {@code AuthManager} to be used.
	 */
	public static final String AUTHMANAGER_ID = "authManagerId";
	/**
	 * Id of the {@code TimeLevelTemplateManager} to be used.
	 */
	public static final String TIMETEMPLATEMANAGER_ID = "timeTemplateManagerId";
	/**
	 * Id of the {@code PreProcessor} to be used.
	 */
	public static final String PREPROCESSOR_ID = "preProcessorId";
	/**
	 * The name of the module for the exceptionRegistry
	 */
	public static final String EXCEPTIONREGISTRY_ID = IConfiguration.coreExceptionRegistryId;

	/**
	 * Access method to the id via reflection used for XSLT process.
	 * 
	 * @param id
	 *            the id to get the value for
	 * 
	 * @return the value of the static field named {@code id} of
	 *         {@code DefaultValue}
	 */
	public static String getId(final String id) {
		try {
			final Field idField = DefaultValues.class.getField(id);
			return (String) idField.get(null);
		} catch (final Exception e) {
			return null;
		}
	}

	/**
	 * Gets the default location to be used if no other is defined.
	 * 
	 * @return the default location
	 */
	public static String getDefaultLocation() {
		return "_data";
	}

	/**
	 * Gets the default {@code BitmapCache} implementation to be used.
	 * 
	 * @return the default cache implementation to be used
	 */
	public static String getDefaultBitmapCache() {
		return MemoryBitmapCache.class.getName();
	}

	/**
	 * Gets the default {@code AuthManager} implementation to be used.
	 * 
	 * @return the default auth-manager implementation to be used
	 * 
	 * @see IAuthManager
	 */
	public static String getDefaultAuthManager() {
		return AllAccessAuthManager.class.getName();
	}

	/**
	 * Gets the default {@code FactDescriptorModelSetCache} implementation to be
	 * used.
	 * 
	 * @return the default cache implementation to be used
	 */
	public static String getDefaultFactSetsCache() {
		return MemoryFactDescriptorModelSetCache.class.getName();
	}

	/**
	 * Gets the default {@code DataRecordsCache} implementation to be used.
	 * 
	 * @return the default cache implementation to be used
	 */
	public static String getDefaultRecordsCache() {
		return IdsOnlyDataRecordCache.class.getName();
	}

	/**
	 * Gets the default {@code IdentifierCache} implementation to be used.
	 * 
	 * @return the default cache implementation to be used
	 */
	public static String getDefaultIdentifierCache() {
		return MemoryIdentifierCache.class.getName();
	}

	/**
	 * Gets the default {@code MetaDataCache} implementation to be used.
	 * 
	 * @return the default cache implementation to be used
	 */
	public static String getDefaultMetaDataCache() {
		return MemoryMetaDataCache.class.getName();
	}

	/**
	 * Gets the default {@code IndexFactory} to be used.
	 * 
	 * @return the default {@code IndexFactory} to be used
	 * 
	 * @see BaseIndexFactory
	 */
	public static String getDefaultIndexFactory() {
		return IndexFactory.class.getName();
	}

	/**
	 * Gets the default {@code MapperFactory} to be used.
	 * 
	 * @return the default {@code MapperFactory} to be used
	 * 
	 * @see BaseMapperFactory
	 */
	public static String getDefaultMappersFactory() {
		return MapperFactory.class.getName();
	}

	/**
	 * Get the default {@code TimeGranularityFactory} to be used.
	 * 
	 * @return the {@code TimeGranularityFactory} to be used if none is defined
	 * 
	 * @see ITimeGranularityFactory
	 */
	public static String getDefaultGranularitiesFactory() {
		return TimeGranularityFactory.class.getName();
	}

	/**
	 * Get the default {@code QueryFactory} to be used.
	 * 
	 * @return the {@code QueryFactory} to be used if none is defined
	 * 
	 * @see IQueryFactory
	 */
	public static String getDefaultQueryFactory() {
		return QueryFactory.class.getName();
	}

	/**
	 * Gets the class of the {@code IdFactory} to be used for the descriptors.
	 * 
	 * @return the class of the {@code IdFactory} to be used for the descriptors
	 * 
	 * @see IIdsFactory
	 */
	public static String getDefaultIdFactory() {
		return IntegerIdsFactory.class.getName();
	}

	/**
	 * Determines the {@code DataRetriever} to be used for the specified
	 * {@code id}.
	 * 
	 * @param id
	 *            the id to determine a default {@code DataRetriever} for
	 * 
	 * @return the {@code Clazz} of the {@code DataRetriever} to use, can be
	 *         also {@code null} if no default is known for the specified
	 *         {@code id}
	 */
	public static String getDefaultDataRetriever(final String id) {

		// check if an id starts with the specified value
		if (id != null) {
			for (final Entry<String, String> e : getDataRetrievers().entrySet()) {
				if (id.startsWith(e.getKey() + "_")) {
					return e.getValue();
				}
			}
		}

		return null;
	}

	/**
	 * Get the default {@code TimeGranularity} used for the
	 * {@code TimelineDefinition} if non is defined.
	 * 
	 * @return the default {@code TimeGranularity} used for the
	 *         {@code TimelineDefinition}
	 * 
	 * @see ITimeGranularity
	 * @see TimelineDefinition
	 */
	public static String getDefaultGranularity() {
		return getDefaultGranularityInstance().getClass().getName();
	}

	/**
	 * Gets the instance of the default {@code TimeGranularity} used for the
	 * {@code TimelineDefinition} if non is defined.
	 * 
	 * @return the default {@code TimeGranularity} used for the
	 *         {@code TimelineDefinition}
	 * 
	 * @see ITimeGranularity
	 * @see TimelineDefinition
	 */
	public static ITimeGranularity getDefaultGranularityInstance() {
		return Second.instance();
	}

	/**
	 * The default {@code DataRetrievers} available.
	 * 
	 * @return a map which associates the identifiers of the {code
	 *         DataRetrievers} with the implementation
	 */
	public static Map<String, String> getDataRetrievers() {
		final Map<String, String> d = new LinkedHashMap<String, String>();

		d.put("db", DbDataRetriever.class.getName());
		d.put("fxd", FixedStructureDataRetriever.class.getName());
		d.put("csv", CsvDataRetriever.class.getName());

		return d;
	}

	/**
	 * Gets the comma-separated-classes of the default aggregation functions.
	 * 
	 * @return the comma-separated-classes of the default aggregation functions
	 */
	public static String getDefaultAggregationFunctions() {
		final StringBuilder res = new StringBuilder();

		String separator = "";
		for (final Class<? extends IAggregationFunction> clazz : getAggregationFunctions()) {
			res.append(separator);
			res.append(clazz.getName());
			separator = ",";
		}

		return res.toString();
	}

	/**
	 * Method to get the classes of the default concrete
	 * {@code IAggregationFunction}.
	 * 
	 * @return the classes of the default concrete {@code IAggregationFunction}
	 */
	public static List<Class<? extends IAggregationFunction>> getAggregationFunctions() {
		final List<Class<? extends IAggregationFunction>> functions = new ArrayList<Class<? extends IAggregationFunction>>();

		functions.add(Count.class);
		functions.add(Max.class);
		functions.add(Min.class);
		functions.add(Mean.class);
		functions.add(Median.class);
		functions.add(Mode.class);
		functions.add(Sum.class);
		functions.add(CountStarted.class);
		functions.add(CountFinished.class);

		return functions;
	}

	/**
	 * Determines the class to be used for the specified {@code type}.
	 * 
	 * @param type
	 *            the type to specify the class for
	 * 
	 * @return the string representation of the class to be used for the
	 *         {@code type}
	 */
	public static String determineTypeClass(final String type) {
		if ("int".equalsIgnoreCase(type)) {
			return "int";
		} else if ("long".equalsIgnoreCase(type)) {
			return "long";
		} else if ("double".equalsIgnoreCase(type)) {
			return "double";
		} else if ("date".equalsIgnoreCase(type)) {
			return Date.class.getName();
		} else if ("bigInt".equalsIgnoreCase(type)) {
			return BigInteger.class.getName();
		} else if ("bigDec".equalsIgnoreCase(type)) {
			return BigDecimal.class.getName();
		} else if (Classes.getClass(type, false) != null) {
			return type;
		} else {
			throw new IllegalArgumentException(
					"The type '"
							+ type
							+ "' is not supported please try to specify the class using the class attribute.");
		}
	}

	/**
	 * Determines the class to be used for the specified {@code value}.
	 * 
	 * @param value
	 *            the value to specify the class for
	 * 
	 * @return the string representation of the class to be used for the
	 *         {@code value}
	 */
	public static String determineValueClass(final String value) {

		if (value == null || "".equals(value.trim())) {
			return String.class.getName();
		} else if (Strings.isInteger(value) != null) {
			return "int";
		} else if (Strings.isLong(value) != null) {
			return "long";
		} else if (Strings.isDouble(value) != null) {
			return "double";
		} else if (Strings.isBigInteger(value) != null) {
			return BigInteger.class.getName();
		} else if (Strings.isBigDecimal(value) != null) {
			return BigDecimal.class.getName();
		} else if (Dates.isDate(value) != null) {
			return Date.class.getName();
		} else {
			return String.class.getName();
		}
	}
}
