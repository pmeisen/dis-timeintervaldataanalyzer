package net.meisen.dissertation.bugs.bug0002;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.resources.Resource;
import net.meisen.general.genmisc.resources.ResourceInfo;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperties;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * This implementations checks the performance of loading persisted caches based
 * on the {@code BaseFileBitmapIdCache}. The performance seems to be really bad,
 * in the present model.<br />
 * <br/>
 * A first analysis should, that the performance decreases dramatically when
 * increasing the cache size.<br />
 * <br/>
 * Further profiling and debugging should that the
 * {@code RecentlyUsedCachingStrategy} seems to be the bottleneck. Thus, we
 * implemented a {@code RandomCachingStrategy}. The implementation speed was
 * increased dramatically and therefore we decided to keep it random for now.
 * 
 * @author pmeisen
 * 
 */
@SystemProperties(value = {
		@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/bugs/bug0002/tidaConfigBug0002.xml"),
		@SystemProperty(property = "test.folder", value = "TestCallsLoadingPerformance") })
public class TestCallsLoadingPerformance extends LoaderBasedTest {
	private final static File TESTFOLDER = new File(
			System.getProperty("java.io.tmpdir")
					+ "TestCallsLoadingPerformance");

	/**
	 * Initialize the test.
	 * 
	 * @throws IOException
	 *             if the files could not be loaded
	 */
	@BeforeClass
	public static void init() throws IOException {
		final File tidaFolder = new File(TESTFOLDER, "tidadata");

		// cleanup any old test
		Files.deleteDir(TESTFOLDER);
		tidaFolder.mkdirs();

		// create the structure
		final Pattern p = Pattern
				.compile(".*net[\\\\/]meisen[\\\\/]dissertation[\\\\/]bugs[\\\\/]bug0002[\\\\/]tidadata[\\\\/](.*)");
		final Collection<ResourceInfo> res = Resource.getResources(p, true,
				false);
		for (final ResourceInfo r : res) {
			final Matcher m = p.matcher(r.getFullPath());
			m.find();

			final File destFile = new File(tidaFolder, m.group(1));
			destFile.getParentFile().mkdirs();
			Files.copyResourceToFile(r, destFile);
		}
	}

	/**
	 * Clean-up after the test.
	 */
	@AfterClass
	public static void cleanUp() {
		Files.deleteDir(TESTFOLDER);
	}

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	/**
	 * The test printing the performances.
	 */
	@Test
	public void testPerformance() {
		TidaModel model = loader.getTidaModel("callsModel");
		if (model == null) {
			model = loader.loadFromDefaultLocation("callsModel");
		}
		assertEquals(63824, model.getAmountOfRecords());

		IQuery query;
		SelectResultTimeSeries res;

		Performance p = new Performance();
		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF MAX(COUNT(recipient)) AS C ON TIME.RASTER.DAY FROM callsModel IN [01.11.2013, 01.12.2013)");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF MAX(COUNT(recipient)) AS C ON TIME.RASTER.DAY FROM callsModel IN [01.11.2013, 01.12.2013)");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF COUNT(recipient) AS C ON TIME.RASTER.DAY FROM callsModel IN [01.06.2014, 30.06.2014]");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF SUM(recipient) AS C ON TIME.RASTER.DAY FROM callsModel IN [01.06.2014, 30.06.2014]");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF SUM(rate) AS C FROM callsModel IN [29.09.2013, 30.09.2013) GROUP BY CALLER.GENDER.TYPE");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF SUM(rate) AS C ON TIME.RASTER.DAY FROM callsModel IN [29.09.2013, 30.09.2013) GROUP BY CALLER.GENDER.TYPE");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF MAX(SUM(rate)) AS C ON TIME.RASTER.DAY FROM callsModel IN [29.09.2013, 30.09.2013) GROUP BY CALLER.GENDER.TYPE");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

		p.start(true);
		query = queryFactory
				.parseQuery("SELECT TIMESERIES OF SUM(MEDIAN(rate)) AS C ON TIME.RASTER.DAY  FROM callsModel IN [01.09.2013, 31.09.2013) GROUP BY CALLER.GENDER.TYPE");
		res = queryFactory.evaluateQuery(query, null);
		System.out.println(p.printSecs(p.stop(true)));
		System.out.println(res);

	}
}
