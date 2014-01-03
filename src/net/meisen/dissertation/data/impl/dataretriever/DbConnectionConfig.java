package net.meisen.dissertation.data.impl.dataretriever;

import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;

public class DbConnectionConfig implements IDataRetrieverConfiguration {

	private String type;
	private String url;
	private String driver;
	private String username;
	private String password;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getDriver() {
		return driver;
	}

	public void setDriver(String driver) {
		this.driver = driver;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

}
