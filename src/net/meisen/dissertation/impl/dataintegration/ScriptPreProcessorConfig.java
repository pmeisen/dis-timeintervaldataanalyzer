package net.meisen.dissertation.impl.dataintegration;

import net.meisen.dissertation.model.dataintegration.IPreProcessorConfig;

/**
 * Configuration of a {@code ScriptPreProcessor}.
 * 
 * @author pmeisen
 * 
 */
public class ScriptPreProcessorConfig implements IPreProcessorConfig {
	private String script;
	private String language;

	/**
	 * Default constructor, initializing nothing.
	 */
	public ScriptPreProcessorConfig() {
		this(null);
	}

	/**
	 * Constructor used to specify the language of the script.
	 * 
	 * @param language
	 *            the language of the script
	 */
	public ScriptPreProcessorConfig(final String language) {
		this.language = language;
	}

	/**
	 * Gets the language of the script specified.
	 * 
	 * @return the language of the script specified
	 */
	public String getLanguage() {
		return language;
	}

	/**
	 * Sets the language of the script.
	 * 
	 * @param language
	 *            the language of the script
	 */
	public void setLanguage(final String language) {
		this.language = language;
	}

	/**
	 * Gets the script.
	 * 
	 * @return the script
	 */
	public String getScript() {
		return script;
	}

	/**
	 * Sets the script.
	 * 
	 * @param script
	 *            the script
	 */
	public void setScript(final String script) {
		this.script = script;
	}
}
