package net.meisen.dissertation.impl.parser.query.revoke;

/**
 * The types which can be revoked.
 * 
 * @author pmeisen
 * 
 */
public enum RevokeType {
	/**
	 * Type specifying that permissions of a user are revoked.
	 */
	USER,
	/**
	 * Type specifying that permissions of a role are revoked.
	 */
	ROLE;
}
