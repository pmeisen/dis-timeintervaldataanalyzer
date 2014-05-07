package net.meisen.dissertation.model.persistence;

/**
 * A persistable factory marks an {@code Persistable} to be a factory. That
 * means when loading persisted data, the factory is called if an
 * {@code Identifier} is found, which contains the factory's group. The
 * following example demonstrates the behavior.<br/>
 * <br/>
 * <b>Example:</b><br/>
 * Let us assume that the {@code PersistableFactory} is registered for the
 * {@code Group} ({@code "my", "path"}). If an {@code Identifier} is found
 * during the loading process with e.g. (
 * {@code "my", "path", "1", "identifier.data"}) the factory is informed to
 * create an instance. The factory has to determine if an instance has to be
 * created, or if it already is created. Additionally, the factory can register
 * the created instance to be loaded, because factories are triggered prior to
 * the actual call of
 * {@link #load(BasePersistor, Identifier, java.io.InputStream)}.
 * 
 * 
 * @author pmeisen
 * 
 * @see IPersistable
 * @see Identifier
 * @see Group
 * 
 */
public interface IPersistableFactory extends IPersistable {

	/**
	 * Called whenever the {@code Identifier} has the prefix of the factory's
	 * {@code Group}.
	 * 
	 * @param persistor
	 *            the persistor loading
	 * @param identifier
	 *            the {@code Identifier} triggering the call
	 */
	public void createInstance(final BasePersistor persistor,
			final Identifier identifier);

}
