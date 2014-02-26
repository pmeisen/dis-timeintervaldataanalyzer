package net.meisen.dissertation.parser.query;

import org.junit.Test;

public class TestQueryFactory {

	@Test
	public void testStringParsing() {
		final QueryFactory factory = new QueryFactory();
		
		factory.parseQuery("SELECT TIMELINES IN (500 ,600)");
		
		factory.parseQuery("SELECT TIMELINES IN (15.06.2014 , 15.06.2015 00:00]");
	}
}
