package de.z0rdak.yawp.platform.config;

import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.platform.services.config.IRegionConfigHelper;

import java.util.Set;

public class FabricRegionConfigHelper implements IRegionConfigHelper {

    @Override
    public boolean shouldActivateNewDimRegion() {
        return RegionConfig.shouldActivateNewDimRegion();
    }

    @Override
    public int getPaginationSize() {
        return RegionConfig.getPaginationSize();
    }

    @Override
    public int getDefaultPriority() {
        return RegionConfig.getDefaultPriority();
    }

    @Override
    public int getDefaultPriorityInc() {
        return RegionConfig.getDefaultPriorityInc();
    }

    @Override
    public Set<String> getDefaultFlags() {
        return RegionConfig.getDefaultFlags();
    }

    @Override
    public Set<String> getDefaultDimFlags() {
        return RegionConfig.getDefaultDimFlags();
    }
}