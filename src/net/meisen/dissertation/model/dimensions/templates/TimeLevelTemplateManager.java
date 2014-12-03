package net.meisen.dissertation.model.dimensions.templates;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TimeTemplateManagerException;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Manager used to make the different templates available. The manager is used
 * to retrieve templates of levels.
 * 
 * @author pmeisen
 * 
 */
public class TimeLevelTemplateManager {

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private final Map<String, ITimeLevelTemplate> templates;

	/**
	 * Default constructor adding the default instances.
	 */
	public TimeLevelTemplateManager() {
		this.templates = new HashMap<String, ITimeLevelTemplate>();

		addTemplate(new All());
		addTemplate(new Years());
		addTemplate(new Months());
		addTemplate(new Days());
		addTemplate(new Hours());
		addTemplate(new Minutes());
		addTemplate(new Seconds());
		addTemplate(new Rasters());
	}

	/**
	 * Adds a template to the manager.
	 * 
	 * @param template
	 *            the template or factory to be added
	 * 
	 * @see ITimeLevelTemplateFactory
	 */
	public void addTemplate(final ITimeLevelTemplate template) {
		if (template == null) {
			return;
		}

		final String id;
		if (template instanceof ITimeLevelTemplateFactory
				&& !((ITimeLevelTemplateFactory) template).isConcreteTemplate()) {
			id = ((ITimeLevelTemplateFactory) template).getPrefixId();
		} else {
			id = template.getId();
		}

		// check if there is another template
		final ITimeLevelTemplate curTemplate = this.templates.get(id);
		if (curTemplate != null && !curTemplate.equals(template)) {
			exceptionRegistry.throwRuntimeException(
					TimeTemplateManagerException.class, 1000, id);
		}

		this.templates.put(id, template);
	}

	/**
	 * Gets the template with the specified {@code id}.
	 * 
	 * @param granularityFactory
	 *            the {@code TimeGranularityFactory}
	 * @param id
	 *            the template with the specified id
	 * 
	 * @return the template or {@code null} if no template was found
	 */
	public ITimeLevelTemplate getTemplate(
			final TimeGranularityFactory granularityFactory, final String id) {
		return getTemplate(granularityFactory, null, id);
	}

	/**
	 * Gets the template with the specified {@code id} using the specified
	 * factory (if {@code factoryId} is not {@code null}).
	 * 
	 * @param granularityFactory
	 *            the {@code TimeGranularityFactory}
	 * @param factoryId
	 *            the factory to be used
	 * @param id
	 *            the identifier of the template
	 * 
	 * @return the created template
	 */
	protected ITimeLevelTemplate getTemplate(
			final TimeGranularityFactory granularityFactory,
			final String factoryId, final String id) {
		ITimeLevelTemplate template = this.templates.get(factoryId == null ? id
				: factoryId);

		if (template == null && factoryId == null) {
			final String[] parts = id.split(Pattern
					.quote(ITimeLevelTemplateFactory.PREFIX_SEPARATOR));
			if (parts.length > 1) {
				template = getTemplate(granularityFactory, parts[0], id);
			}
		} else if (template instanceof ITimeLevelTemplateFactory
				&& !((ITimeLevelTemplateFactory) template).isConcreteTemplate()) {
			final ITimeLevelTemplateFactory factory = (ITimeLevelTemplateFactory) template;
			template = factory.createTemplate(granularityFactory, id);
			addTemplate(template);
		}

		return template;
	}
}
