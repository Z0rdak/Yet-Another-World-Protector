package de.z0rdak.yawp.platform.services.event;

import de.z0rdak.yawp.api.events.region.YawpEvents;
import net.minecraft.server.level.ServerLevel;

public interface YawpEventDispatcher {

    default void post(ServerLevel level) {
        YawpEvents.ON_REGION_DATA_LOADED.invoke(cb -> cb.onDataLoaded(level));    }
}
