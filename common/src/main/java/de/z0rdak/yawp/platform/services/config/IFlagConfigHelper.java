package de.z0rdak.yawp.platform.services.config;

import java.util.Set;

public interface IFlagConfigHelper {

    Set<String> getCoveredBlockEntities();
    Set<String> getCoveredBlockEntityTags();
    boolean removeEntitiesEnabled();
    boolean isDisabledByConfig(String flag);
}
