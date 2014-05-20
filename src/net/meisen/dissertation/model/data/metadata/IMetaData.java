package net.meisen.dissertation.model.data.metadata;

import java.util.Collection;

public interface IMetaData {

	public String getDescriptorModelId();
	
	public Collection<Object> getValues();
}
