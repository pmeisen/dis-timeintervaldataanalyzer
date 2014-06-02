package net.meisen.dissertation.impl.cache;

import java.util.Arrays;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IIdentifierCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Base implementation of a {@code IdentifierCache} which just implements the
 * base stuff to keep the values in memory.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseIdentifierCache implements IIdentifierCache {

	/**
	 * The {@code ExceptionRegistry} used to handle exceptions.
	 * 
	 * @see IExceptionRegistry
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * The {@code IndexFactory} used to create indexed, in this case especially
	 * bitmap-indexes.
	 * 
	 * @see BaseIndexFactory
	 */
	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	protected BaseIndexFactory indexFactory;

	private boolean init = false;
	private boolean persistency = true;

	private int lastUsedIdentifier;
	private Bitmap bitmap;

	/**
	 * The base implementation sets the default values for the
	 * {@code lastUsedIdentifier} and the {@code bitmap}.
	 */
	@Override
	public void initialize(final TidaModel model) {
		initialize(-1, indexFactory.createBitmap());
		markAsInitialized();
	}

	@Override
	public int getLastUsedIdentifier() {
		return this.lastUsedIdentifier;
	}

	@Override
	public Bitmap getValidIdentifiers() {
		return bitmap;
	}

	@Override
	public void markIdentifierAsUsed(final int lastUsedIdentifier) {
		if (!isInitialized()) {
			exceptionRegistry.throwException(getExceptionClass(1000), 1000);
		} else if (lastUsedIdentifier <= getLastUsedIdentifier()) {
			exceptionRegistry.throwException(getExceptionClass(1001), 1001,
					lastUsedIdentifier, getLastUsedIdentifier());
		}

		if (isPersistencyEnabled()) {
			cacheIdentifier(lastUsedIdentifier);
		}

		this.lastUsedIdentifier = lastUsedIdentifier;
	}

	/**
	 * Checks if the {@code identifier}-array contains only allowed identifiers.
	 * An identifier of the array is assumed to be allowed, if it's smaller or
	 * equal the {@code lastUsedIdentifier}.
	 * 
	 * @param identifier
	 *            the array to check
	 * 
	 * @return {@code true} if the array contains only allowed identifiers,
	 *         otherwise {@code false}
	 */
	protected boolean checkIdentifier(final int[] identifier) {
		Arrays.sort(identifier);
		final int max = identifier[identifier.length - 1];

		return max <= lastUsedIdentifier;
	}

	@Override
	public void markIdentifierAsValid(final int... validIdentifier) {
		if (!isInitialized()) {
			exceptionRegistry.throwException(getExceptionClass(1000), 1000);
		} else if (validIdentifier == null || validIdentifier.length < 1) {
			return;
		} else if (!checkIdentifier(validIdentifier)) {
			exceptionRegistry.throwException(getExceptionClass(1002), 1002);
		}

		// create a new Bitmap with all the valid identifiers set
		final Bitmap newBitmap = getValidIdentifiers().or(
				Bitmap.createBitmap(indexFactory, validIdentifier));

		// update the persisted bitmap
		if (isPersistencyEnabled()) {
			cacheBitmap(newBitmap);
		}

		this.bitmap = newBitmap;
	}

	@Override
	public void markIdentifierAsValid(final Bitmap bitmap) {
		if (bitmap == null) {
			return;
		}

		// create a new Bitmap with all specified identifiers
		final Bitmap newBitmap = getValidIdentifiers().or(bitmap);

		// update the persisted bitmap
		if (isPersistencyEnabled()) {
			cacheBitmap(newBitmap);
		}

		this.bitmap = newBitmap;
	}

	@Override
	public void markIdentifierAsInvalid(final int... invalidIdentifier) {
		if (!isInitialized()) {
			exceptionRegistry.throwException(getExceptionClass(1000), 1000);
		} else if (invalidIdentifier == null || invalidIdentifier.length < 1) {
			return;
		} else if (!checkIdentifier(invalidIdentifier)) {
			exceptionRegistry.throwException(getExceptionClass(1002), 1002);
		}

		// create a new Bitmap with all the valid identifiers set
		Bitmap markerBitmap = Bitmap.createBitmap(indexFactory,
				invalidIdentifier);

		// use not-logical to invert the bitmap partly
		markerBitmap = markerBitmap.invert(lastUsedIdentifier);

		// now use and-logical to get the once still set
		final Bitmap newBitmap = getValidIdentifiers().and(markerBitmap);

		// update the persisted bitmap
		if (isPersistencyEnabled()) {
			cacheBitmap(newBitmap);
		}

		this.bitmap = newBitmap;
	}

	/**
	 * This method should always be called by the concrete implementation of
	 * {@link #initialize(TidaModel)}. If no concrete implementation is used,
	 * the base-implementation calls the method with default values. to
	 * initialize
	 * 
	 * @param lastUsedIdentifier
	 *            the value to be used as last-used-identifier
	 * @param bitmap
	 *            the bitmap to be used initially
	 */
	protected void initialize(final int lastUsedIdentifier, final Bitmap bitmap) {
		this.lastUsedIdentifier = lastUsedIdentifier;
		this.bitmap = bitmap;
	}

	/**
	 * Checks if the cache is initialized.
	 * 
	 * @return {@code true} if the cache is initialized, otherwise {@code false}
	 */
	public boolean isInitialized() {
		return init;
	}

	/**
	 * Method to set the cache to be initialized.
	 */
	protected void markAsInitialized() {
		this.init = true;
	}

	/**
	 * Caches the specified {@code newBitmap} representing the valid
	 * identifiers.
	 * 
	 * @param newBitmap
	 *            the {@code Bitmap} to be cached
	 */
	protected abstract void cacheBitmap(final Bitmap newBitmap);

	/**
	 * Caches the specified {@code lastUsedIdentifier}.
	 * 
	 * @param lastUsedIdentifier
	 *            the identifier to be cached
	 */
	protected abstract void cacheIdentifier(final int lastUsedIdentifier);

	/**
	 * Method used to disable or enable (default) the persistency. During the
	 * time the persistency is disable all changes are just done within the
	 * memory. Whenever the persistency is enabled again, all data are persisted
	 * to the hard-drive.
	 * 
	 * @param enable
	 *            {@code true} to enable persistency, otherwise {@code false}
	 * 
	 * @return the old value of the persistency setting
	 */
	@Override
	public boolean setPersistency(final boolean enable) {

		final boolean oldPersistency = this.persistency;
		this.persistency = enable;

		// nothing to do, nothing was changed
		if (oldPersistency == this.persistency) {
			// nothing to do
		}
		// persistency was enabled
		else if (oldPersistency) {
			// nothing to do
		}
		// persistency was disabled, write the not persisted once now
		else {
			cacheBitmap(bitmap);
			cacheIdentifier(lastUsedIdentifier);
		}

		return oldPersistency;
	}

	/**
	 * Checks if the persistency is currently enabled.
	 * 
	 * @return {@code true} if persistency is enabled, otherwise {@code false}
	 */
	public boolean isPersistencyEnabled() {
		return persistency;
	}

	/**
	 * The type of the exception to be thrown via the {@link #exceptionRegistry}
	 * for the specified {@code errorNr}.
	 * 
	 * @param errorNr
	 *            the number to determine the {@code Class} for
	 * 
	 * @return the class of the exception to be thrown
	 */
	protected Class<? extends BaseIdentifierCacheException> getExceptionClass(
			final int errorNr) {
		return BaseIdentifierCacheException.class;
	}

	/**
	 * The base-implementation just resets the initialization value.
	 */
	@Override
	public void release() {
		if (!isInitialized()) {
			return;
		} else {
			init = false;

			// persists everything a last time
			if (!isPersistencyEnabled()) {
				cacheIdentifier(lastUsedIdentifier);
				cacheBitmap(bitmap);
			}

			lastUsedIdentifier = -1;
			bitmap = null;
		}
	}

	@Override
	public String toString() {
		return getLastUsedIdentifier() + " - " + getValidIdentifiers();
	}
}
