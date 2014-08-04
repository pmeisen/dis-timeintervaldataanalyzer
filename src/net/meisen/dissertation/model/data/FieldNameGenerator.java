package net.meisen.dissertation.model.data;

/**
 * A {@code FieldNameGenerator} is used to generate pre-defined field-names. The
 * field-names are reserved and should not be used otherwise.
 * 
 * @author pmeisen
 * 
 */
public class FieldNameGenerator {
	private static FieldNameGenerator fieldNameGenerator = null;

	/**
	 * Gets the one and only {@code FieldNameGenerator}.
	 * 
	 * @return the one and only {@code FieldNameGenerator}
	 */
	public static FieldNameGenerator get() {
		if (fieldNameGenerator == null) {
			fieldNameGenerator = new FieldNameGenerator();
		}
		return fieldNameGenerator;
	}

	/**
	 * Gets the name used for the start value of the interval.
	 * 
	 * @return the name used for the start value of the interval
	 */
	public String getIntervalStartFieldName() {
		return _generateReservedField("START");
	}

	/**
	 * Gets the name used for the end value of the interval.
	 * 
	 * @return the name used for the end value of the interval
	 */
	public String getIntervalEndFieldName() {
		return _generateReservedField("END");
	}

	/**
	 * Gets the name used for the labels of values.
	 * 
	 * @return the name used for the labels of values
	 */
	public String getLabelFieldName() {
		return _generateReservedField("LABEL");
	}

	/**
	 * Gets the name used for the field containing the identifier.
	 * 
	 * @return the name used for the field containing the identifier
	 */
	public String getIdFieldName() {
		return _generateReservedField("ID");
	}

	/**
	 * Gets the name used for the field containing the labeled value.
	 * 
	 * @return the name used for the field containing the labeled value
	 */
	public String getValueFieldName() {
		return _generateReservedField("VALUE");
	}

	/**
	 * Gets the name used for the field containing the raw-value used for
	 * labeling.
	 * 
	 * @return the name used for the field containing the raw-value used for
	 *         labeling
	 */
	public String getRawLabelFieldName() {
		return _generateReservedField("RAWVALUE");
	}

	/**
	 * Gets the name used for the field containing the count-value.
	 * 
	 * @return the name used for the field containing the count-value
	 */
	public String getCountFieldName() {
		return _generateReservedField("COUNT");
	}

	/**
	 * Gets the name used for the field containing the model-id-value.
	 * 
	 * @return the name used for the field containing the model-id-value
	 */
	public String getModelIdFieldName() {
		return _generateReservedField("MODELID");
	}

	/**
	 * Gets the name used for the field containing the db-title-value.
	 * 
	 * @return the name used for the field containing the db-title-value
	 */
	public String getDbTitleFieldName() {
		return _generateReservedField("DBIMPLTITLE");
	}

	/**
	 * Gets the name used for the field containing the db-version-value.
	 * 
	 * @return the name used for the field containing the db-version-value
	 */
	public String getDbVersionFieldName() {
		return _generateReservedField("DBIMPLVERSION");
	}

	/**
	 * Gets the name used for the field containing the build-value.
	 * 
	 * @return the name used for the field containing the build-value
	 */
	public String getInternalRevisionFieldName() {
		return _generateReservedField("DBIMPLBUILD");
	}

	/**
	 * Gets the name used for the field containing the user-names.
	 * 
	 * @return the name used for the field containing the user-names
	 */
	public String getUserFieldName() {
		return _generateReservedField("USERNAME");
	}

	/**
	 * Gets the name used for the field containing the roles of an entity.
	 * 
	 * @return the name used for the field containing the roles of an entity
	 */
	public String getRolesFieldName() {
		return _generateReservedField("ROLES");
	}

	/**
	 * Gets the name used for the field containing the permissions of an entity.
	 * 
	 * @return the name used for the field containing the permissions of an
	 *         entity
	 */
	public String getPermissionsFieldName() {
		return _generateReservedField("PERMISSIONS");
	}

	/**
	 * Gets the name used for the field containing a permission.
	 * 
	 * @return the name used for the field containing a permission
	 */
	public String getPermissionFieldName() {
		return _generateReservedField("PERMISSION");
	}

	private String _generateReservedField(final String resWord) {
		return "[" + resWord + "]";
	}
}
