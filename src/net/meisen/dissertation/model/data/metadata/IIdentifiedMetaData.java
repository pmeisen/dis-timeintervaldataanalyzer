package net.meisen.dissertation.model.data.metadata;

import java.util.Map;

public interface IIdentifiedMetaData extends IMetaData {
	
	public Map<Object, Object> getIdentifiedValues();
}
