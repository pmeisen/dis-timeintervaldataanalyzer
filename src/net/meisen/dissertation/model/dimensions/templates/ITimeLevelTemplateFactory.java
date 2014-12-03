package net.meisen.dissertation.model.dimensions.templates;

import net.meisen.dissertation.model.time.granularity.ITimeGranularityFactory;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Interface used for a {@code ITimeLevelTemplateFactory}. A factory has to
 * implement the {@code TimeLevelTemplate} interface as well.
 * 
 * @author pmeisen
 * 
 */
public interface ITimeLevelTemplateFactory extends ITimeLevelTemplate {

	/**
	 * The separator used by the {@code TimeLevelTemplateManager} to identify
	 * the prefix (i.e. the {@code creatorId}) from the identifier of a
	 * template.<br/>
	 * <br/>
	 * <b>Example:</b><br/>
	 * <ul>
	 * <li></li>
	 * </ul>
	 */
	public final static String PREFIX_SEPARATOR = "_";

	/**
	 * The prefix which identifier a template to be created by the factory.
	 * 
	 * @return the prefix identifier
	 */
	public String getPrefixId();

	/**
	 * Method to validate if the factory is really usable for the specified
	 * template.
	 * 
	 * @param id
	 *            the identifier to be checked
	 * 
	 * @return {@code true} if the factory can be used, otherwise {@code false}
	 */
	public boolean isCreator(final String id);

	/**
	 * Defines if the specified instance is a really created template or just
	 * the factory. It might be, that the factory and an create instance share
	 * the same type, and thereby this distinguishing method is necessary.
	 * 
	 * @return {@code true} if it's a concrete instance and not the factory
	 *         itself
	 */
	public boolean isConcreteTemplate();

	/**
	 * Creates the template for the specified {@code id}.
	 * 
	 * @param factory
	 *            the factory used to create {@code TimeGranularity} instances
	 *            based on strings
	 * @param id
	 *            the identifier of the template to be created
	 * 
	 * @return the created template
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an error occurs during creation
	 */
	public ITimeLevelTemplate createTemplate(
			final ITimeGranularityFactory factory, final String id)
			throws ForwardedRuntimeException;
}
