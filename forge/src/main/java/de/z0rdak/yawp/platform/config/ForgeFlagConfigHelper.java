package de.z0rdak.yawp.platform.config;

import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.platform.services.config.IFlagConfigHelper;

import java.util.Set;

public class ForgeFlagConfigHelper implements IFlagConfigHelper {

    @Override
    public Set<String> getCoveredBlockEntities() {
        return FlagConfig.getCoveredBlockEntities();
    }

    @Override
    public Set<String> getCoveredBlockEntityTags() {
        return FlagConfig.getCoveredBlockEntityTags();
    }

    @Override
    public boolean removeEntitiesEnabled() {
        return FlagConfig.removeEntitiesEnabled();
    }
}