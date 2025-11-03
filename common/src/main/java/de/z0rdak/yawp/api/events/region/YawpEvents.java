package de.z0rdak.yawp.api.events.region;

import de.z0rdak.yawp.platform.services.event.EventBus;
import net.minecraft.server.level.ServerLevel;

public final class YawpEvents {
    @FunctionalInterface public interface RegionDataLoadListener {
        void onDataLoaded(ServerLevel level);
    }

    public static final EventBus<RegionDataLoadListener> ON_REGION_DATA_LOADED = new EventBus<>();

}