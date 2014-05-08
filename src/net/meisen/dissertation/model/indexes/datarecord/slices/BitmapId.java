package net.meisen.dissertation.model.indexes.datarecord.slices;

import com.google.common.base.Objects;

import net.meisen.dissertation.model.indexes.datarecord.IDataRecordIndex;
import net.meisen.general.genmisc.types.Strings;

public class BitmapId<I> {

	private final I id;
	private final Class<? extends IDataRecordIndex> type;
	private String[] classifier;

	public BitmapId(final I id, final Class<? extends IDataRecordIndex> type,
			final String... classifier) {
		this.id = id;
		this.type = type;
		this.classifier = classifier;
	}

	@SuppressWarnings("unchecked")
	public Class<I> getIdType() {
		return (Class<I>) id.getClass();
	}

	public I getId() {
		return id;
	}

	@Override
	public String toString() {
		final String c = Strings.join(".", classifier);

		if (c.isEmpty()) {
			return c + "." + id + " (" + type.getSimpleName() + ")";
		} else {
			return id + " (" + type.getSimpleName() + ")";
		}
	}

	@Override
	public int hashCode() {
		if (classifier == null || classifier.length == 0) {
			return Objects.hashCode(id, type);
		} else {
			return Objects.hashCode(id, type, classifier);
		}
	}
}
